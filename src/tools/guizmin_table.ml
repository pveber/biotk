(* open BatEnum.Infix *)
open Core.Std
open Guizmin

type 'a format

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

let with_rows 
    (type row) (type obj) (module T : T with type Row.t = row)
    ?(header = false) ?(sep = '\t') 
    (File file) ~f =
  In_channel.with_file file ~f:(fun ic ->
    line_stream_of_channel ic
    |> (if header then BatStream.drop 1 else ident)
    |> BatStream.map (fun x -> T.Row.of_array (split ~on:sep x))
    |> f
  )

let with_obj_rows
    (type obj) (module T : T with type Obj.t = obj)
    ?header ?sep
    file ~f =
  with_rows (module T) file ~f:(fun xs ->
    f (BatStream.map T.Obj.of_row xs)
  )

let load
    (type table) (module T : T with type Table.t = table)
    ?line_numbers ?header ?sep (File f) =
  T.Table.of_file ?line_numbers ?header ?sep f

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

module Make(X : T) =
struct
  type row = X.Row.t
  type obj = X.Obj.t
  type table = X.Table.t
  type 'a ty = 'a X.Row.ty

  type file = unit ty format Guizmin.file
  type file_path = unit ty format Guizmin.file_path

  type 'a file' = 'a ty format Guizmin.file
  type 'a file_path' = 'a ty format Guizmin.file_path

  let with_rows ?header ?sep file ~f = with_rows (module X) ?header ?sep file ~f
  let with_obj_rows ?header ?sep file ~f = with_obj_rows (module X) ?header ?sep file ~f
  let load ?header ?sep file = load (module X) ?header ?sep file
end





























