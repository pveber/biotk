open Core.Std
open Guizmin

type ty
type file = ty Guizmin.file

let contents (File fa) =
  Core.In_channel.with_file
    fa
    ~f:Biocaml_stream.(fun ic ->
      Biocaml.Fasta.Exceptionful.in_channel_to_char_seq_item_stream ~pedantic:false ic
      |! map ~f:Biocaml.Fasta.(fun it -> it.header, it.sequence)
    )

















