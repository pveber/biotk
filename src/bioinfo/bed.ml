open Guizmin
open MBSchema

(* type 'a ty *)
(* type 'a file_path = 'a ty Guizmin_table.file_path *)
(* type 'a file = 'a ty Guizmin_table.file *)

(* let basic_parse ?header (path : 'a file_path) = *)
(*   Guizmin_table.parse ?header  *)
(*     (fun (Guizmin_table.Line f) ->  *)
(*       Location.make f.(0) (int_of_string f.(1)) (int_of_string f.(2))) *)
(*     path *)

(* type track *)


type table minimal = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int 
}

type table stranded = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
  score : float ;
  strand : [`sense "+" | `antisense "-"] ;
}

type table compact = {
  loc : Location
}

include Guizmin_table.NEWAPI.Impl

type minimal_file = Minimal.f file
module type Minimal_sig = module type of Minimal
let minimal : Minimal.f ty = (module Minimal)

type stranded_file = Stranded.f file
let stranded : Stranded.f ty = (module Stranded)

(*
type 'a ty
type 'a file_path = 'a ty Guizmin_table.file_path
type 'a file = 'a ty Guizmin_table.file

val basic_parse : ?header:bool -> 'a file_path -> Location.t BatEnum.t
*)

type track










