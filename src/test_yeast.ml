open Batteries
open Guizmin
(* open Guizmin_bioinfo *)

(* module Genome = struct *)
(*   let sequence = Ucsc.genome_sequence `sacCer2 *)
(* end *)

(* let _ = ignore (eval Genome.sequence) *)

(* let _ = Misc.wget "xx" *)

let _ = v2

let _ =
  value pipeline "mon.pipeline.2.0"
    ~(x:int) ?(y:string = "1") z:string
    uses foo bar* -> (
      1 + 1
    )
