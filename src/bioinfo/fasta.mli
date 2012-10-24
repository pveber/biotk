open Guizmin

type ty
type file = ty Guizmin.file

val contents : ty file_path -> (string * string) BatEnum.t
