open Guizmin

type format
type file = format Guizmin.file
type file_path = format Guizmin.file_path

val with_file : ?tags:Biocaml_gff.Tags.t -> file_path -> f:(Biocaml_gff.item Stream.t -> 'a) -> 'a
