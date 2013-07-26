(* open BatEnum.Infix *)
open Core.Std
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

let with_rows 
    (type row) (module R : Row with type t = row)
    ?(header = false) ?(sep = '\t') 
    (File file) ~f =
  In_channel.with_file file ~f:(fun ic ->
    line_stream_of_channel ic
    |> (if header then BatStream.drop 1 else ident)
    |> BatStream.map (fun x -> R.of_array (split ~on:sep x))
    |> f
  )
    
let load
    (type row) (type table)
    (module R : Row with type t = row)
    (module T : Table with type t = table)
    ?line_numbers ?header ?sep (File f) =
  T.of_file ?line_numbers ?header ?sep f

module MakeOpen(R : Row)(T : Table) =
struct
  let with_rows ?header ?sep file ~f = with_rows (module R) ?header ?sep file ~f
  let load ?header ?sep file = load (module R) (module T) ?header ?sep file
end

module Make(R : Row)(T : Table) =
struct
  type file = R.t format Guizmin.file
  type file_path = R.t format Guizmin.file_path

  include MakeOpen(R)(T)
end



















