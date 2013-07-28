open Guizmin
open Guizmin_bioinfo

type meme_chip_output
val meme_chip : 
  ?meme_nmotifs:int -> 
  ?meme_minw:int ->
  ?meme_maxw:int ->
  Fasta.file -> meme_chip_output dir
val meme_chip_meme_motifs : meme_chip_output dir_path -> (string * Biocaml_pwm.count_matrix) list

val meme2images : ?rc:unit -> [`meme_motif] file -> [`meme2image_output] dir










