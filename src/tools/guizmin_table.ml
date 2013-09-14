(* open BatEnum.Infix *)
open Core.Std
open Guizmin

type ('row,'comment,'header) format

let count_occurences ch s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = ch then incr accu
  done ;
  !accu

let split ~on x =
  let n = String.length x in
  let m = count_occurences on x + 1 in
  let res = Array.create ~len:m "" in
  let rec search k i j =
    if j >= n then res.(k) <- String.sub x i (j - i)
    else (
      if x.[j] = on then (
	res.(k) <- String.sub x i (j - i) ;
	search (k + 1) (j + 1) (j + 1)
      )
      else search k i (j + 1)
    )
  in
  search 0 0 0 ;
  res

let line_stream_of_channel ic =
  let f _ =
    try Some (input_line ic)
    with End_of_file -> None
  in
  Stream.from f


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

module Sharp_comment = struct
  type t = [`sharp]
  let char = '#'
end

module No_header = struct
  type t = [`none]
  let kind = `none
end

module Some_header = struct
  type t = [`some]
  let kind = `some
end

let with_rows
    (type row) (type obj) (module Row : Row with type t = row)
    ?(header = false) ?(sep = '\t') ?(comment_char = '\000')
    (File file) ~f =
  In_channel.with_file file ~f:(fun ic ->
    line_stream_of_channel ic
    |> (if header then BatStream.drop 1 else ident)
    |> BatStream.filter (fun x -> not (x = "" || x.[0] = comment_char))
    |> BatStream.map (fun x -> Row.of_array (split ~on:sep x))
    |> f
  )

let with_obj_rows
    (type row) (type obj) (module Row : Row with type t = row) (module Obj : Obj with type t = obj and type row = Row.t)
    ?header ?sep ?comment_char
    file ~f =
  with_rows (module Row) ?header ?sep ?comment_char file ~f:(fun xs ->
    f (BatStream.map Obj.of_row xs)
  )

let load
    (type table) (module Table : Table with type t = table)
    ?line_numbers ?header ?sep ?comment_char (File f) =
  Table.of_file ?line_numbers ?header ?sep ?comment_char f

module Make(Row : Row)(Obj : Obj with type row = Row.t)(Table : Table)(Comment : Comment)(Header : Header) =
struct
  type file = (unit Row.ty, Comment.t, Header.t) format Guizmin.file
  type file_path = (unit Row.ty, Comment.t, Header.t) format Guizmin.file_path

  type 'a file' = ('a Row.ty, Comment.t, Header.t) format Guizmin.file
  type 'a file_path' = ('a Row.ty, Comment.t, Header.t) format Guizmin.file_path

  let with_rows file ~f = with_rows (module Row) ~header:(Header.kind <> `none) ~comment_char:Comment.char file ~f
  let with_obj_rows file ~f = with_obj_rows (module Row) (module Obj) ~header:(Header.kind <> `none) ~comment_char:Comment.char file ~f
  let load file = load (module Table) ~header:(Header.kind <> `none) ~comment_char:Comment.char file
end

let remove_comments sep header file =
  f1
    "guizmin.tools.remove_sharp_comments[r1]"
    Param.([ string "sep" (Char.to_string sep) ])
    file
    (
      fun env (File file) path ->
        In_channel.with_file file ~f:(fun ic ->
          line_stream_of_channel ic
          |! BatStream.filter (fun line -> String.length line > 0 && line.[0] <> sep)
          |! (if header then BatStream.drop 1 else ident)
          |! fun xs -> Out_channel.with_file path ~f:(fun oc ->
            BatStream.iter
              (fun line -> output_string oc line ; output_char oc '\n')
              xs
          )
        )
    )

let remove_sharp_comments file = remove_comments '#' false file
let remove_sharp_comments_and_header file = remove_comments '#' true file

external red3 : ('a * ('b * ('c * 'd)), 'e, 'f) format file -> ('a * ('b * ('c * unit)), 'e, 'f) format file = "%identity"
