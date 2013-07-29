open Guizmin
open MBSchema

module Basic : sig
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }
  include module type of Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.No_comment_nor_header)

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
  include module type of Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.No_comment_nor_header)

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

  include module type of Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.No_comment_nor_header)

  val location_of_row : Row.t -> Location.t
end

type 'a file = 'a Basic.file'
type 'a named_file = 'a Named.file'
type 'a stranded_file = 'a Stranded.file'


type track


















