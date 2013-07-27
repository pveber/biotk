open Guizmin
open MBSchema

module Basic : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }

  include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty

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

  include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty

  val make : ?prefix:string -> 'a Basic.file' -> file
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

  include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty

  val location_of_row : Row.t -> Location.t
end

type 'a file = 'a Basic.file'
type 'a named_file = 'a Named.file'
type 'a stranded_file = 'a Stranded.file'


type track


















