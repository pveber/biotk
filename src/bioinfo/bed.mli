open Guizmin
open MBSchema

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int 
}

type 'a format constraint 'a = #Obj.t

module Basic : sig
  type file' = Obj.t format Guizmin_table.file
  type file_path' = Obj.t format Guizmin_table.file_path
      
  type 'a file = (#Obj.t as 'a) format Guizmin_table.file
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
  val of_locations : Location.t list pipeline -> file'
end

module With_name : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }

  type file' = Obj.t format Guizmin_table.file
  type file_path' = Obj.t format Guizmin_table.file_path

  type 'a file = (#Obj.t as 'a) format Guizmin_table.file
  type 'a file_path = (#Obj.t as 'a) format Guizmin_table.file_path

  val with_rows : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(Row.t Stream.t -> 'b) -> 'b

  val load : 
    ?header:bool ->
    ?sep:char ->
    'a file_path -> Table.t

  val make : ?prefix:string -> 'a Basic.file -> file'
  (** keeps the first three cols and adds a fourth with a generated
      identifier. This is necessary for certain routines to work,
      notably sequence retrieval *)
    
  val location_of_row : Row.t -> Location.t
end

module With_strand : sig

  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : [`sense "+" | `antisense "-"] ;
  }

  type file' = Obj.t format Guizmin_table.file
  type file_path' = Obj.t format Guizmin_table.file_path

  type 'a file = (#Obj.t as 'a) format Guizmin_table.file
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

include module type of Basic

type track


















