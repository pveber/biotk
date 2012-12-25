open Guizmin
open MBSchema

module Minimal = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }
  type file = Obj.t Guizmin_table.file
  include Guizmin_table.Make(Row)(Table)
end

module Named = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  type file = Obj.t Guizmin_table.file
  include Guizmin_table.Make(Row)(Table)
end

module Stranded = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : [`sense "+" | `antisense "-"] ;
  }
  type file = Obj.t Guizmin_table.file
  include Guizmin_table.Make(Row)(Table)
end

type 'a file = (#Minimal.Obj.t as 'a) Guizmin_table.file
type 'a named_file = (#Named.Obj.t as 'a) Guizmin_table.file
type 'a stranded_file = (#Stranded.Obj.t as 'a) Guizmin_table.file

type track

















