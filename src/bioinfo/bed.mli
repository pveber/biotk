open Guizmin
open MBSchema

type 'a format constraint 'a = < chrom : string ; chromStart : int ; chromEnd : int ; .. >
type 'a file = 'a format Guizmin_table.file
type 'a basic_file = 'a file
type 'a named_file = 'a file constraint 'a = < name : string ; .. >
type 'a stranded_file = 'a named_file constraint 'a = < strand : [`sense | `antisense ] ; .. >

module Basic : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }

  type file = Obj.t format Guizmin_table.file
  type 'a file_path = (#Obj.t as 'a) format Guizmin_table.file_path
      
  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(Row.t Stream.t -> 'b) -> 'b
    
  val with_rows_obj : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(Obj.t Stream.t -> 'b) -> 'b
    
  val load : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> Table.t
    
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

  type file = Obj.t format Guizmin_table.file
  type 'a file_path = (#Obj.t as 'a) format Guizmin_table.file_path

  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(Row.t Stream.t -> 'b) -> 'b

  val load : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> Table.t

  val make : ?prefix:string -> 'a basic_file -> file
  (** keeps the first three cols and adds a fourth with a generated
      identifier. This is necessary for certain routines to work,
      notably sequence retrieval *)
    
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

  type file = Obj.t format Guizmin_table.file
  type 'a file_path = (#Obj.t as 'a) format Guizmin_table.file_path

  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(Row.t Stream.t -> 'b) -> 'b

  val load : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> Table.t

  val location_of_row : Row.t -> Location.t
end

type track


















