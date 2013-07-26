open Guizmin

module type Row = sig
  type t
  val labels : string list
  val of_array : string array -> t
end

module type Table = sig
  type t
  val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> string -> t
end

type 'a format
type 'a file = 'a format Guizmin.file
type 'a file_path = 'a format Guizmin.file_path

(* val with_rows : *)
(*   (module Row with type t = 'a) -> *)
(*   ?header:bool -> *)
(*   ?sep:char -> *)
(*   'a file_path -> f:('a Stream.t -> 'b) -> 'b *)
  
(* val load :  *)
(*   (module Row with type t = 'a) -> *)
(*   (module Table with type t = 'b) -> *)
(*   ?line_numbers:bool -> ?header:bool -> ?sep:char -> 'a file_path -> 'b *)

(** Functor for closed type tables *)
module Make : 
  functor (R : Row) -> 
    functor (T : Table) -> 
sig
  type file = R.t format Guizmin.file
  type file_path = R.t format Guizmin.file_path

  val with_rows :
    ?header:bool ->
    ?sep:char ->
    file_path -> f:(R.t Stream.t -> 'b) -> 'b

  val load : ?header:bool -> ?sep:char -> file_path -> T.t
end

(** Functor for hierarchies of table files *)
module MakeOpen : 
  functor (R : Row) -> 
    functor (T : Table) -> 
sig
  val with_rows :
    ?header:bool ->
    ?sep:char ->
    'a format Guizmin.file_path -> f:(R.t Stream.t -> 'b) -> 'b

  val load : ?header:bool -> ?sep:char -> 'a format Guizmin.file_path -> T.t
end

















