open BatEnum.Infix
open Core
open Guizmin

type 'a line = Line of string array
type ('a, 'b) line_parser = 'a line -> 'b
type 'a file_path = 'a line Guizmin.file_path
type 'a file = 'a line Guizmin.file

let ( |> ) = BatPervasives.( |> )

let count_occurences ch s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    if s.[i] = ch then incr accu
  done ;
  !accu 
    
let split ~on x = 
  let n = String.length x in 
  let m = count_occurences on x + 1 in 
  let res = Array.make m "" in
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

let input_line ic =
  split ~on:'\t' (input_line ic)

let output_line oc = function
| [| |] -> output_char oc '\n'
| l -> 
    output_string oc l.(0) ;
    for i = 1 to Array.length l - 1 do
      output_char oc '\t' ;
      output_string oc l.(i) ;
    done ;
    output_char oc '\n'

let map id f file = 
  f1
    ("guizmin.tools.table", [string "mapping" id])
    (fun _ (File input) output ->
      In_channel.with_file input ~f:(fun ic ->
        Out_channel.with_file output ~f:(fun oc ->
          try 
            let i = ref 0 in
            while true do
              output_line oc (f !i (input_line ic)) ;
              incr i
            done 
          with End_of_file -> ()
        )
      )
    )
    file

let parse ?(header = false) lp (File f) =
  (BatFile.lines_of f
     // (fun l -> not (Core_string.is_prefix ~prefix:"#" l))
     |> BatEnum.skip (if header then 1 else 0))
  /@ split ~on:'\t'
  /@ (fun x -> lp (Line x))

module NEWAPI = struct
  module type Format = sig
    type row
    type table
    (* Signature *)
    type s
  end

  type ('s,'r,'t) full_format = (module Format with type s = 's and type row = 'r and type table = 't)
  type 's constrained_format = ('s, 'r, 't) full_format constraint 's = < row : 'r ; table : 't >

  class type ['a] row = object
    method row : 'a
  end

  class type ['a] table = object
    method table : 'a
  end

  module type S = sig
    type 'a format = private 'a constrained_format
    type 'a file_path = 'a format Guizmin.file_path
    type 'a file = 'a format Guizmin.file

    val to_stream : ('a #row as 'b) format -> 'b file_path -> 'a Stream.t
    val load : ('a #table as 'b) format -> 'b file_path -> 'a
  end

  module Impl = struct
    type 'a format = 'a constrained_format
    type 'a file_path = 'a format Guizmin.file_path
    type 'a file = 'a format Guizmin.file
        
    let to_stream (format : ('a #row as 'b) format) (File f : 'b file_path) =
      assert false
        
    let load (format : ('a #table as 'b) format) (File f : 'b file_path) =
      assert false
  end

  include Impl
end



















