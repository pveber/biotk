open Base
open Stdio
open Unsigned
open Signed

type chunk = {
  chunk_beg : UInt64.t ;
  chunk_end : UInt64.t ;
}

type bin = {
  bin : int ;
  n_chunk : int ;
  chunks : chunk array ;
}

type interval = Ioffset of UInt64.t [@@unboxed]

type reference_sequence = {
  n_bin : int ;
  bins : bin array ;
  n_intv : int ;
  intervals : interval array ;
}

type t = {
  n_ref : int ;
  reference_sequences : reference_sequence array ;
  n_no_coor : UInt64.t option ;
}

exception Parser_error of string

let fail msg = raise (Parser_error msg)

let failf fmt =
  Printf.ksprintf fail fmt

let input_byte t =
  Option.value_exn (In_channel.input_byte t)

let input_s32 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  let open Int32 in
  logor (of_int b1)
    (logor (shift_left (of_int b2) 8)
       (logor (shift_left (of_int b3) 16)
          (shift_left (of_int b4) 24)))

let input_u64 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  let b5 = input_byte ic in
  let b6 = input_byte ic in
  let b7 = input_byte ic in
  let b8 = input_byte ic in
  let open UInt64 in
  (shift_left (of_int b8) 56)
  |> logor (shift_left (of_int b7) 48)
  |> logor (shift_left (of_int b6) 40)
  |> logor (shift_left (of_int b5) 32)
  |> logor (shift_left (of_int b4) 24)
  |> logor (shift_left (of_int b3) 16)
  |> logor (shift_left (of_int b2)  8)
  |> logor (of_int b1)

let input_s32_as_int context ic =
  match Base.Int32.to_int (input_s32 ic) with
  | Some i -> i
  | None -> failf "Met too big an integer while parsing a %s" context

let read_magic_string ic =
  let buf = Bytes.create 4 in
  In_channel.really_input_exn ic ~buf ~pos:0 ~len:4 ;
  if String.(buf <> "BAI\001") then fail "Incorrect magic string"

let read_chunk ic =
  let chunk_beg = input_u64 ic in
  let chunk_end = input_u64 ic in
  { chunk_beg ; chunk_end }

let read_bin ic =
  let bin = input_s32_as_int "bin" ic in
  let n_chunk = input_s32_as_int "n_chunk" ic in
  let chunks = Array.init n_chunk ~f:(fun _ -> read_chunk ic) in
  { bin ; n_chunk ; chunks }

let read_interval ic = Ioffset (input_u64 ic)

let read_reference_sequence ic =
  let n_bin = input_s32_as_int "n_bin" ic in
  let bins = Array.init n_bin ~f:(fun _ -> read_bin ic) in
  let n_intv = input_s32_as_int "n_intv" ic in
  let intervals = Array.init n_intv ~f:(fun _ -> read_interval ic) in
  { n_bin ; bins ; n_intv ; intervals }

let read_reference_sequences ic n =
  Array.init n ~f:(fun _ -> read_reference_sequence ic)

let read ic =
  try
    read_magic_string ic ;
    let n_ref = input_s32_as_int "n_ref" ic in
    let reference_sequences = read_reference_sequences ic n_ref in
    let n_no_coor = try Some (input_u64 ic) with End_of_file -> None in
    Ok { n_ref ; reference_sequences ; n_no_coor }
  with Parser_error msg -> Error (`Msg msg)

let%test "read" =
  match In_channel.with_file "../data/ex1.bam.bai" ~f:read with
  | Ok r ->
    r.n_ref = 2
  | Error _ -> false
