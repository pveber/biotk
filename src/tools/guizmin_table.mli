open Guizmin

type 'a line = private Line of string array
type ('a, 'b) line_parser = 'a line -> 'b
type 'a file_path = 'a line Guizmin.file_path
type 'a file = 'a line Guizmin.file

val map : string -> (int -> string array -> string array) -> 'a file -> 'b file

val parse : ?header:bool -> ('a,'b) line_parser -> 'a file_path -> 'b BatEnum.t




















