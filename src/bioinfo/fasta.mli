open Guizmin

type format
type file = format Guizmin.file

val with_contents : format file_path -> f:((string * string) Stream.t -> 'a) -> 'a
val contents : format file_path -> (string * string) list
val sequences : format file_path -> string list
