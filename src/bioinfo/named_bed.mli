(** Bed file with four columns *)

open Guizmin
open MBSchema

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
}
type file' = Obj.t Guizmin_table.file
type file_path' = Obj.t Guizmin_table.file_path

type 'a file = (#Obj.t as 'a) Guizmin_table.file
type 'a file_path = (#Obj.t as 'a) Guizmin_table.file_path

val with_rows : 
  ?header:bool ->
  ?sep:char ->
  'a file_path -> f:(Row.t Stream.t -> 'b) -> 'b

val make : ?prefix:string -> 'a Bed.file -> file'
(** keeps the first three cols and adds a fourth with a generated
    identifier. This is necessary for certain routines to work,
    notably sequence retrieval *)
  
val location_of_row : Row.t -> Location.t
