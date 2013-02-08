open Guizmin

type format
type file = format Guizmin.file
type file_path = format Guizmin.file_path

val with_file : file_path -> f:(Biocaml_gff.stream_item Stream.t -> 'a) -> 'a
