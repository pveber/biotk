open Batteries
open Guizmin
open Guizmin_bioinfo

module Genome = struct
  let sequence = Ucsc.genome_sequence `sacCer2
end

let _ = ignore (eval Genome.sequence)
