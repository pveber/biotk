open Core.Std
open CFStream
open Guizmin

type format
type file = format Guizmin.file

let with_contents (File fa) ~f =
  In_channel.with_file
    fa
    ~f:Stream.(fun ic ->
      Biocaml.Fasta.in_channel_to_char_seq_item_stream_exn ic
      |! map ~f:Biocaml.Fasta.(fun it -> it.header, it.sequence)
      |! f
    )

let contents fa  = with_contents ~f:Stream.to_list fa
let sequences fa = with_contents ~f:(fun xs -> Stream.(to_list (map xs snd))) fa




















