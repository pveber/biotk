open Guizmin

type ty
type file = ty Guizmin.file

val with_contents : ty file_path -> f:((string * string) Stream.t -> 'a) -> 'a
