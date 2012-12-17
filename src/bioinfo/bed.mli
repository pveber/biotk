open Guizmin
open MBSchema

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

include Guizmin_table.NEWAPI.S

type minimal_file = Minimal.f file
val minimal : Minimal.f ty
val stranded : Stranded.f ty

(*
type 'a ty
type 'a file_path = 'a ty Guizmin_table.file_path
type 'a file = 'a ty Guizmin_table.file

val basic_parse : ?header:bool -> 'a file_path -> Location.t BatEnum.t
*)

type track
