open Guizmin
open MBSchema

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
  score : float ;
  strand : [`sense "+" | `antisense "-"] ;
}
type file = Obj.t Guizmin_table.file
type file_path = Obj.t Guizmin_table.file_path

type 'a file' = (#Obj.t as 'a) Guizmin_table.file
type 'a file_path' = (#Obj.t as 'a) Guizmin_table.file_path

include Guizmin_table.MakeOpen(Row)(Table)

let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
