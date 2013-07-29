open Guizmin

type ('row,'comment,'header) format

module type Row = sig
  type t
  type 'a ty
  val labels : string list
  val of_array : string array -> t
end

module type Obj = sig
  type t
  type row
  val of_row : row -> t
  val to_row : t -> row
end

module type Table = sig
  type t
  val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> string -> t
end

module type Layout = sig
  type comment
  type header
  val header : bool
end

module No_comment_nor_header : Layout with type comment = [`none]
                                      and  type header  = [`none]

module Sharp_comment_no_header : Layout with type comment = [`sharp]
                                        and  type header  = [`none]

module Make : 
  functor (Row : Row) ->
    functor (Obj : Obj with type row = Row.t) ->
      functor (Table : Table) ->
        functor (Layout : Layout) ->
sig
  type file = (unit Row.ty, Layout.comment, Layout.header) format Guizmin.file
  type file_path = (unit Row.ty, Layout.comment, Layout.header) format Guizmin.file_path

  type 'a file' = ('a Row.ty, Layout.comment, Layout.header) format Guizmin.file
  type 'a file_path' = ('a Row.ty, Layout.comment, Layout.header) format Guizmin.file_path

  val with_rows :
    'a file_path' -> f:(Row.t Stream.t -> 'b) -> 'b

  val with_obj_rows :
    'a file_path' -> f:(Obj.t Stream.t -> 'b) -> 'b

  val load : 'a file_path' -> Table.t
end


