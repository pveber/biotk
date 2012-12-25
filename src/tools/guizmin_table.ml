(* open BatEnum.Infix *)
open Core.Std
open Guizmin

include Guizmin_table_sig

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
    f (BatStream.map (fun x -> R.of_array (split ~on:sep x)) (line_stream_of_channel ic))
  )
    
let load
    (type row) (type table)
    (module R : Row with type t = row)
    (module T : Table with type t = table)
    ?(header = false) ?(sep = '\t') (File f) = assert false


module Make(R : Row)(T : Table) =
struct
  let with_rows ?header ?sep file ~f = with_rows (module R) ?header ?sep file ~f
  let load ?header ?sep file = load (module R) (module T) ?header ?sep file
end


(* let with_file ?(header = false) ?(sep = '\t') row_of_array (File file) ~f = *)
(*   In_channel.with_file file ~f:(fun ic -> *)
(*     f (BatStream.map (fun x -> row_of_array (split ~on:sep x)) (line_stream_of_channel ic)) *)
(*   ) *)

(* let save ?(header = false) ?(sep = '\t') path row_to_array xs = assert false *)

(* module Make(R : Row)(F : Format) = struct *)
(*   open Core.Std *)

(*   type row = R.t *)
(*   type format = F.t *)
(*   type file_path = < row : row ; format : format > contents Guizmin.file_path *)
(*   type file = < row : row ; format : format > contents Guizmin.file *)

(*   let with_file ?(header = false) ?(sep = '\t') x ~f =  *)
(*     with_file ~header ~sep R.of_array x ~f *)

(*   let save ?(header = false) ?(sep = '\t') path xs =  *)
(*     save ~header ~sep path (assert false) xs *)
    
(* end *)


(* (\* let ( |> ) = BatPervasives.( |> ) *\) *)


(* (\* let input_line ic = *\) *)
(* (\*   split ~on:'\t' (input_line ic) *\) *)

(* (\* let output_line oc = function *\) *)
(* (\* | [| |] -> output_char oc '\n' *\) *)
(* (\* | l ->  *\) *)
(* (\*     output_string oc l.(0) ; *\) *)
(* (\*     for i = 1 to Array.length l - 1 do *\) *)
(* (\*       output_char oc '\t' ; *\) *)
(* (\*       output_string oc l.(i) ; *\) *)
(* (\*     done ; *\) *)
(* (\*     output_char oc '\n' *\) *)

(* (\* let map id f file =  *\) *)
(* (\*   f1 *\) *)
(* (\*     ("guizmin.tools.table", [string "mapping" id]) *\) *)
(* (\*     (fun _ (File input) output -> *\) *)
(* (\*       In_channel.with_file input ~f:(fun ic -> *\) *)
(* (\*         Out_channel.with_file output ~f:(fun oc -> *\) *)
(* (\*           try  *\) *)
(* (\*             let i = ref 0 in *\) *)
(* (\*             while true do *\) *)
(* (\*               output_line oc (f !i (input_line ic)) ; *\) *)
(* (\*               incr i *\) *)
(* (\*             done  *\) *)
(* (\*           with End_of_file -> () *\) *)
(* (\*         ) *\) *)
(* (\*       ) *\) *)
(* (\*     ) *\) *)
(* (\*     file *\) *)

(* (\* let parse ?(header = false) lp (File f) = *\) *)
(* (\*   (BatFile.lines_of f *\) *)
(* (\*      // (fun l -> not (Core_string.is_prefix ~prefix:"#" l)) *\) *)
(* (\*      |> BatEnum.skip (if header then 1 else 0)) *\) *)
(* (\*   /@ split ~on:'\t' *\) *)
(* (\*   /@ (fun x -> lp (Line x)) *\) *)





(* module NEWAPI = struct *)
(*   open Core.Std *)

(*   module type Format = sig *)
(*     type row *)
(*     type table *)
(*     type s *)
(*     val stream_of_channel :  *)
(*       ?line_numbers:bool -> *)
(*       ?header:bool -> *)
(*       ?sep:char -> *)
(*       in_channel -> row Stream.t *)
(*   end *)

(*   type ('s,'r,'t) full_format = (module Format with type s = 's and type row = 'r and type table = 't) *)
(*   type 's constrained_format = ('s, 'r, 't) full_format constraint 's = < row : 'r ; table : 't > *)

(*   class type ['a] row = object *)
(*     method row : 'a *)
(*   end *)

(*   class type ['a] table = object *)
(*     method table : 'a *)
(*   end *)

(*   module type S = sig *)
(*     type 'a format = private 'a constrained_format *)
(*     type 'a file_path = 'a format Guizmin.file_path *)
(*     type 'a file = 'a format Guizmin.file *)

(*     val stream_of_channel : 'a #row format -> in_channel -> 'a Stream.t *)
(*     val stream_to_file : ('a #row as 'b) format -> string -> 'a Stream.t -> unit *)
(*     val load : ('a #table as 'b) format -> 'b file_path -> 'a *)
(*     val save : ('a #table as 'b) format -> string -> 'a -> unit *)
(*   end *)

(*   module Impl = struct *)
(*     type 'a format = 'a constrained_format *)
(*     type 'a file_path = 'a format Guizmin.file_path *)
(*     type 'a file = 'a format Guizmin.file *)
        
(*     let stream_of_channel *)
(*         (type row) (type table)  *)
(*         (module F : Format with type s = < row : row ; table : table >  *)
(*                            and type row = row *)
(*                            and type table = table)  *)
(*         ic  *)
(*         = *)
(*       F.stream_of_channel ic *)
        
(*     let stream_to_file *)
(*         (type row) (type table)  *)
(*         (module F : Format with type s = < row : row ; table : table >  *)
(*                            and type row = row *)
(*                            and type table = table)  *)
(*         path *)
(*         = *)
(*       assert false *)

(*     let load (format : ('a #table as 'b) format) (File f : 'b file_path) = *)
(*       assert false *)

(*     let save (format : ('a #table as 'b) format) path = *)
(*       assert false *)

(*   end *)

(*   include Impl *)
(* end *)














