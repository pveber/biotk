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

type table named = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
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

type minimal_file = Minimal.s file
let minimal : Minimal.s format = (module Minimal)

type named_file = Named.s file
let named : Named.s format = (module Named)

type stranded_file = Stranded.s file
let stranded : Stranded.s format = (module Stranded)

(*
type 'a ty
type 'a file_path = 'a ty Guizmin_table.file_path
type 'a file = 'a ty Guizmin_table.file

val basic_parse : ?header:bool -> 'a file_path -> Location.t BatEnum.t
*)

type track










