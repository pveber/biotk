open Guizmin


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
end

type 'a file = (#Minimal.Obj.t as 'a) Guizmin_table.file
type 'a named_file = (#Named.Obj.t as 'a) Guizmin_table.file
type 'a stranded_file = (#Stranded.Obj.t as 'a) Guizmin_table.file

type track



















