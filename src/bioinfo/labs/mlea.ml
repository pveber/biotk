open Core.Std
open Printf
open Biocaml_stream.Infix
module Stream = Biocaml_stream
open Guizmin_bioinfo
open Guizmin_bioinfo.MBSchema

let radius = 1000

(** consider broader regions around peaks *)
let bed_recenter ~radius (bed : 'a Bed.With_name.file) : Bed.With_name.file' =
  Guizmin.(
    f1
      "guizmin.bioinfo.labs.mlea.bed_recenter[r1]"
      Param.([ int "radius" radius ])
      bed
      Bed.With_name.(fun env bed path -> 
        with_rows bed ~f:(fun items ->
          items 
          /@ (fun loc -> 
                let center = (loc.chromStart + loc.chromEnd) / 2 in
                { loc with
                        chromStart = max 0 (center - 1000) ;
                        chromEnd   = center + 1000 })
          |! fun xs -> Out_channel.with_file path ~f:(fun oc -> Bed.With_name.Row.stream_to_channel oc xs)
        )
      )
  )

let jaspar_library jl =
  Guizmin.v1 
    "guizmin.bioinfo.labs.mlea.jaspar_library[r2]" []
    jl
    (
      fun _ -> 
        List.map ~f:(
          fun m ->
            Biocaml_jaspar.(
              m.id ^ "/" ^ m.factor_name, 
              Biocaml_pwm.(make m.matrix (flat_background ()))
            )
        )
    )

type result = {
  motif_id : string ;
  pwm : Biocaml_pwm.t ;
  positions : int array ;
  fold : float ;
  pval : float ;
  pval_adj : float ;
  fdr : float
}

let pwm_signal pwm seq =
  Biocaml_pwm.fast_scan pwm seq Float.neg_infinity
  |! List.map ~f:snd
  |! Array.of_list

let sequence_length = function
| [] -> raise (Invalid_argument "empty list")
| h :: t -> 
    let n = String.length h in
    if List.for_all t ~f:(fun x -> String.length x = n) then
      n
    else 
      raise (Invalid_argument "All strings in the list should have the same length")

type test_result =
    < positions : float list R.t ; 
      p'value : float R.t ;
      fold : float R.t > Rbase.dataframe R.t

let single_test preds (motif_id, pwm) =
  let hit_position (st,ed,_,_) =
    float ((st + ed) / 2 - radius)
  in
  let xs = 
    List.map preds ~f:(List.map ~f:hit_position)
    |! List.concat
  in
  if xs = [] then
    (motif_id, pwm, xs, 1., 1.)
  else
    let r : test_result = 
      <:rscript<
        library(stats)
        x <- $v:xs$
        l <- $i:radius * 2$
        mu <- mean(x)
        sigma <- sd(x)
        k <- sum(abs(x - mu) < sigma / 2)
        t <- binom.test(k,length(x),sigma / l, alternative='greater')
        data.frame(positions = x, p.value = t#p.value, fold = t#estimate / t#null.value)
      >> 
    in
    (motif_id, pwm, 
     R.floats_of_t (r ## positions),
     R.float_of_t (r ## fold),
     R.float_of_t (r ## p'value))
      
let graph org bed motif =
  let bed = Ucsc.(bedClip (chrom_info org)) (bed_recenter ~radius bed) in
  let fa = Ucsc.(fasta_of_bed org bed) in
  let preds = Pwm.prediction motif fa in
  Guizmin.(f1
    "guizmin.bioinfo.labs.mlea.graph[r1]" []
    preds 
    (fun env preds path -> 
       let hit_position (st,ed,_,_) =
         float ((st + ed) / 2 - radius)
       in
       let xs = 
         List.map preds ~f:(List.map ~f:hit_position)
         |! List.concat
       in
       <:rscript<
         library(grDevices)
         library(graphics)
         library(stats)
         x <- $v:xs$
         xfit <- seq(min(x),max(x),length=40)
         mu <- mean(x)
         sigma <- sd(x)
         yfit <- dnorm(xfit, mean=mu, sd=sigma)
         pdf($s:path$)
         h <- hist(x,breaks=100,xlab='Position wrt to peak summit',main='',freq=FALSE)
         lines(xfit,yfit,col='red')
         abline(v = - sigma/2, col='red', lty=2)
         abline(v =   sigma/2, col='red', lty=2)
         dev.off()
       >> |! ignore)
  )

let test org bed motifs =
  let bed = Ucsc.(bedClip (chrom_info org)) (bed_recenter ~radius bed) in
  let fa = Ucsc.(fasta_of_bed org bed) in
  let preds = Guizmin.map motifs (fun m -> Pwm.prediction (Guizmin.adapter m snd) fa) in
  Guizmin.(v2
    "guizmin.bioinfo.labs.mlea.test[r3]"
    Param.([ string "org" (Ucsc.string_of_genome org) ])
    preds motifs
    (fun env preds motifs ->
       let raw_results = List.map2_exn preds motifs ~f:single_test in
       let pvals = List.map raw_results ~f:(fun (_,_,_,_,x) -> x) in
       let pval_adj = Rstats.p'adjust ~method_:`bonferroni pvals in
       let fdr = Rstats.p'adjust ~method_:`fdr pvals in
       List.map3_exn raw_results pval_adj fdr ~f:(fun (motif_id, pwm, positions, fold, pval) pval_adj fdr -> {
         positions = List.map positions ~f:Int.of_float |! Array.of_list ; 
         fold ;
         pval ; pval_adj ; fdr ;
         motif_id ; pwm 
       }
       )
    )
  )

module Tsv_output = struct
  module Id = struct
    include String
    let to_string x = sprintf "%20s" x
  end

  module Float = struct
    type t = float
    let to_string x = sprintf "%16.3g" x
    let of_string = Float.of_string
  end

  module Data = struct
    type tabular line = {
      motif_id : Id ;
      fold : Float ;
      pval : Float ;
      pval_adj : Float ;
      fdr : Float
    }
  end

  let of_test r = 
    Guizmin.f1
      "guizmin.bioinfo.labs.mlea.tsv_output.of_test[r6]"
      []
      r
      (fun env results path ->
        let results = List.sort (fun r1 r2 -> - compare r1.fold r2.fold) results in
        Stream.of_list results
        /@ (fun { motif_id ; fold ; pval ; pval_adj ; fdr } -> 
              { Data.motif_id ; fold ; pval ; pval_adj ; fdr })
        |! fun lines -> Out_channel.with_file path ~f:(fun oc -> Data.Row.stream_to_channel oc lines)
      )
  include Data
end

let latex_output r =
  Guizmin.f1
    "guizmin.bioinfo.labs.mlea.latex_output[r1]"
      []
      r
      (
        fun env results path ->
          let module Table = Tsv_output.Data.Table in
          let results = 
            List.sort (fun r1 r2 -> - compare r1.fold r2.fold) results 
            |> List.filter ~f:(fun r -> r.fdr < 1.e-3)
            |> List.map ~f:(fun { motif_id ; fold ; pval ; pval_adj ; fdr } -> 
              { Tsv_output.Data.motif_id ; fold ; pval ; pval_adj ; fdr })
            |> Stream.of_list
            |> Table.of_stream
          in
          Out_channel.with_file path ~f:(fun oc -> Table.latex_to_channel oc results)
      )















