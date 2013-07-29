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
  val of_file : ?line_numbers:bool -> ?header: bool -> ?sep: char -> string -> t
end

module type Layout = sig
  type comment
  type header
  val header : bool
end

module No_comment_nor_header = struct
  type comment = [`none]
  type header  = [`none]
  let header = false
end

module Sharp_comment_no_header = struct
  type comment = [`sharp]
  type header  = [`none]
  let header = false
end



let with_rows 
    (type row) (type obj) (module Row : Row with type t = row)
    ?(header = false) ?(sep = '\t') 
    (File file) ~f =
  In_channel.with_file file ~f:(fun ic ->
    line_stream_of_channel ic
    |> (if header then BatStream.drop 1 else ident)
    |> BatStream.map (fun x -> Row.of_array (split ~on:sep x))
    |> f
  )

let with_obj_rows
    (type row) (type obj) (module Row : Row with type t = row) (module Obj : Obj with type t = obj and type row = Row.t)
    ?header ?sep
    file ~f =
  with_rows (module Row) file ~f:(fun xs ->
    f (BatStream.map Obj.of_row xs)
  )

let load
    (type table) (module Table : Table with type t = table)
    ?line_numbers ?header ?sep (File f) =
  Table.of_file ?line_numbers ?header ?sep f

module Make(Row : Row)(Obj : Obj with type row = Row.t)(Table : Table)(Layout : Layout) =
struct
  type file = (unit Row.ty, Layout.comment, Layout.header) format Guizmin.file
  type file_path = (unit Row.ty, Layout.comment, Layout.header) format Guizmin.file_path

  type 'a file' = ('a Row.ty, Layout.comment, Layout.header) format Guizmin.file
  type 'a file_path' = ('a Row.ty, Layout.comment, Layout.header) format Guizmin.file_path

  let with_rows file ~f = with_rows (module Row) ~header:Layout.header file ~f
  let with_obj_rows file ~f = with_obj_rows (module Row) (module Obj) ~header:Layout.header file ~f
  let load file = load (module Table) ~header:Layout.header file
end





























