open Guizmin

type 'a format

module type T = sig

  module Row : sig
    type t
    type 'a ty
    val labels : string list
    val of_array : string array -> t
  end

  module Obj : sig
    type t
    val of_row : Row.t -> t
    val to_row : t -> Row.t
  end

  module Table : sig
    type t
    val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> string -> t
  end
end

(** Functor for closed type tables *)
module type S = sig
  type row
  type obj
  type table
  type 'a ty

  type file = unit ty format Guizmin.file
  type file_path = unit ty format Guizmin.file_path

  type 'a file' = 'a ty format Guizmin.file
  type 'a file_path' = 'a ty format Guizmin.file_path

  val with_rows :
    ?header:bool ->
    ?sep:char ->
    'a file_path' -> f:(row Stream.t -> 'b) -> 'b

  val with_obj_rows :
    ?header:bool ->
    ?sep:char ->
    'a file_path' -> f:(obj Stream.t -> 'b) -> 'b

  val load : ?header:bool -> ?sep:char -> 'a file_path' -> table
end

module Make : functor (X : T) -> S with type row   = X.Row.t
                                   and  type obj   = X.Obj.t
                                   and  type table = X.Table.t
                                   and  type 'a ty = 'a X.Row.ty




















