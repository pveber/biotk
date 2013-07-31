open Core.Std
open Guizmin

type format
type file = format Guizmin.file

let with_contents (File fa) ~f =
  In_channel.with_file
    fa
    ~f:Biocaml_stream.(fun ic ->
      Biocaml.Fasta.in_channel_to_char_seq_item_stream_exn ic
      |! map ~f:Biocaml.Fasta.(fun it -> it.header, it.sequence)
      |! f
    )

let contents fa  = with_contents ~f:Biocaml.Stream.to_list fa
let sequences fa = with_contents ~f:(fun xs -> Biocaml.Stream.(to_list (map xs snd))) fa




















