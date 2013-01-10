open Core.Std.Fn
open Biocaml_stream.Infix

type score_distribution = float array

let quantile xs =
  Gsl_stats.quantile_from_sorted_data xs

(* adapted from Rosetta code *)
let rec binary_search xs value low high =
  if high = low then
    low
  else let mid = (low + high) / 2 in
    if xs.(mid) > value then
      binary_search xs value low (mid - 1)
    else 
      if xs.(high) <= value then high
      else binary_search xs value mid (high - 1)

let cdf xs x =
  let n = Array.length xs in
  if x < xs.(0) then 0.
  else (
    let k = 1 + binary_search xs x 0 (n - 1) in
    (float_of_int k) /. (float_of_int n)
  )

open Guizmin
open Guizmin_bioinfo

let best_score_distribution_of_fasta pwm fa =
  v2
    "guizmin.bioinfo.labs.pwm.score_distribution_of_fasta[r1]" []
    pwm fa
    (fun env pwm fa ->
      Fasta.with_contents fa ~f:(fun xs ->
        let seqs = Biocaml_stream.to_list (xs /@ snd) in
        let scan pwm =
          List.map (fun x -> Biocaml_pwm.best_hit pwm x |! snd) seqs
        in
        let score_distribution =
          List.map2 max (scan pwm) (scan (Biocaml_pwm.reverse_complement pwm))
          |! Array.of_list        
        in
        Array.sort compare score_distribution ;
        score_distribution
      ))

let const ~id pwm = 
  v0 
    "guizmin.bioinfo.labs.const[r1]" 
    [ Param.string "id" id ] 
    (fun _ -> pwm)

let markov0_control_set fa : Guizmin_bioinfo.Fasta.file = f1
  "guizmin.bioinfo.labs.pwm.markov0_control_set[r1]" []
  fa
  (
    fun _ fa path ->
      Fasta.with_contents fa ~f:(fun seqs ->
        Core.Std.Out_channel.with_file path ~f:(fun oc ->
          Biocaml_stream.iteri
            Fungen.Dnaseq.(control_set markov0 (seqs /@ snd))
            ~f:(Printf.fprintf oc ">seq_%d\n%s\n")
        )
      )
  )

let suppress_superposed_hits l =
  let l = List.sort compare l in
  let rec aux = function
  | [] -> []
  | h :: [] as l -> l
  | ((st1,ed1,_,s1) as h1) :: ((st2,ed2,_,s2) as h2) :: t ->
      if st1 = st2 && ed1 = ed2 then
        let h = if s1 > s2 then h1 else h2 in
        aux (h :: t)
      else
        h1 :: (aux (h2 :: t))
  in
  aux l

let motif_scan pwm theta seq =
  let reshape sense pwm hits =
    let n = Array.length (pwm : Biocaml_pwm.t :> 'a array) in
    List.map (fun (pos, score) -> pos, pos + n, sense, score) hits
  in
  Biocaml_pwm.(
    List.append
      (reshape `sense     pwm (fast_scan pwm                      seq theta))
      (reshape `antisense pwm (fast_scan (reverse_complement pwm) seq theta))
  )
  |! suppress_superposed_hits


let prediction ?(level = 0.1) pwm fa =
  v3
    "guizmin.bioinfo.labs.pwm.prediction[r2]"
    [ Param.float "level" level ]
    pwm fa (best_score_distribution_of_fasta pwm (markov0_control_set fa))
    (fun env pwm fa score_dist ->
       let theta = quantile score_dist (1. -. level) in
       Fasta.with_contents fa ~f:(fun seqs ->
         seqs /@ (fun (_,seq) -> motif_scan pwm theta seq)
         |! Biocaml_stream.to_list
       ))


















