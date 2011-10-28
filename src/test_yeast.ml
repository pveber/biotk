open Batteries
open Guizmin

module Genome = struct
  let sequence = Ucsc.genome_sequence `sacCer2
end

let _ = 
  Makefile.to_channel stdout (compile Genome.sequence)
