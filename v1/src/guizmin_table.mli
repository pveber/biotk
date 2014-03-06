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
  val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> ?comment_char: char -> string -> t
end

module type Comment = sig
  type t
  val char : char
end

module type Header = sig
  type t
  val kind : [`none | `commented | `some]
end

module Sharp_comment : Comment with type t = [`sharp]
module No_header : Header with type t = [`none]
module Some_header : Header with type t = [`some]

module Make :
  functor (Row : Row) ->
    functor (Obj : Obj with type row = Row.t) ->
      functor (Table : Table) ->
        functor (Comment : Comment) ->
          functor (Header : Header) ->
sig
  type file = (unit Row.ty, Comment.t, Header.t) format Guizmin.file
  type file_path = (unit Row.ty, Comment.t, Header.t) format Guizmin.file_path

  type 'a file' = ('a Row.ty, Comment.t, Header.t) format Guizmin.file
  type 'a file_path' = ('a Row.ty, Comment.t, Header.t) format Guizmin.file_path

  val with_rows :
    'a file_path' -> f:(Row.t Stream.t -> 'b) -> 'b

  val with_obj_rows :
    'a file_path' -> f:(Obj.t Stream.t -> 'b) -> 'b

  val load : 'a file_path' -> Table.t
end

val remove_sharp_comments : ('a, [`sharp], 'b) format file -> ('a, [`sharp], 'b) format file
val remove_sharp_comments_and_header : ('a, [`sharp], [`some]) format file -> ('a, [`sharp], [`none]) format file

val red3 : ('a * ('b * ('c * 'd)), 'e, 'f) format file -> ('a * ('b * ('c * unit)), 'e, 'f) format file


















