open Guizmin

type 'a format
type 'a file = 'a format Guizmin.file
type 'a file_path = 'a format Guizmin.file_path

module type Row = sig
  type t
  val labels : string list
  val of_array : string array -> t
end

module type Table = sig
  type t
  val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> string -> t
end

module type F = 
  functor (R : Row) -> 
  functor (T : Table) -> 
sig
  val with_rows :
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(R.t Stream.t -> 'b) -> 'b

  val load : ?header:bool -> ?sep:char -> 'a file_path -> T.t
end

