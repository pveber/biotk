open Batteries
open Guizmin

type ty
type file = ty Guizmin.file

let contents (File fa) =
  Core.In_channel.with_file
    fa
    ~f:BatStream.(fun ic ->
      Biocaml.Fasta.Exceptionful.in_channel_to_char_seq_item_stream ~pedantic:false ic
      |> map Biocaml.Fasta.(fun it -> it.header, it.sequence)
      |> to_list
      |> BatList.enum
    )

















