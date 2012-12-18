open Guizmin
open MBSchema

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

include Guizmin_table.NEWAPI.S

type minimal_file = Minimal.s file
val minimal : Minimal.s format

type named_file = Named.s file
val named : Named.s format

type stranded_file = Stranded.s file
val stranded : Stranded.s format

(*
type 'a ty
type 'a file_path = 'a ty Guizmin_table.file_path
type 'a file = 'a ty Guizmin_table.file

val basic_parse : ?header:bool -> 'a file_path -> Location.t BatEnum.t
*)

type track



















