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

let markov0_control_set fa : Guizmin_bioinfo.Fasta.file = f1
  "guizmin.bioinfo.labs.markov0_control_set[r1]" []
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
