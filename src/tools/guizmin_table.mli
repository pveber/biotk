open Guizmin

type 'a line
type 'a line_parser = string array -> 'a
type 'a file_path = 'a line Guizmin.file_path
type 'a file = 'a line Guizmin.file

val map : string -> (int -> string array -> string array) -> 'a file -> 'b file

val parse : ?header:bool -> 'a line_parser -> 'a file_path -> 'a BatEnum.t








