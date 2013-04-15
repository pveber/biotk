open Guizmin
open MBSchema

module Minimal : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }
  type file = Obj.t Guizmin_table.file
  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    #Obj.t Guizmin_table.file_path -> f:(Row.t Stream.t -> 'b) -> 'b
  val load : 
    ?header:bool ->
    ?sep:char ->
    #Obj.t Guizmin_table.file_path -> Table.t
  val location_of_row : Row.t -> Location.t
  val row_of_location : Location.t -> Row.t
  val of_locations : Location.t list pipeline -> file
end

module Named : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  type file = Obj.t Guizmin_table.file
  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    #Obj.t Guizmin_table.file_path -> f:(Row.t Stream.t -> 'b) -> 'b

  (** keeps the first three cols and adds a fourth with a generated
      identifier. This is necessary for certain routines to work,
      notably sequence retrieval *)
  val make : ?prefix:string -> #Minimal.Obj.t Guizmin_table.file -> file

  val location_of_row : Row.t -> Location.t
end

module Stranded : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : [`sense "+" | `antisense "-"] ;
  }
  type file = Obj.t Guizmin_table.file
  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    #Obj.t Guizmin_table.file_path -> f:(Row.t Stream.t -> 'b) -> 'b

  val location_of_row : Row.t -> Location.t
end

type 'a file = (#Minimal.Obj.t as 'a) Guizmin_table.file
type 'a named_file = (#Named.Obj.t as 'a) Guizmin_table.file
type 'a stranded_file = (#Stranded.Obj.t as 'a) Guizmin_table.file

type track



















