open Guizmin
open Guizmin_bioinfo

module Genome = struct
  let sequence = Ucsc.genome_sequence `sacCer2
end

let eval x = Guizmin.(eval ~base:(default_base_directory ()) x)

let () =
  let File f = eval Genome.sequence in
  print_endline f




















