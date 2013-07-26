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

type file' = Obj.t Bed.format Guizmin_table.file
type file_path' = Obj.t Bed.format Guizmin_table.file_path

type 'a file = (#Obj.t as 'a) Bed.format Guizmin_table.file
type 'a file_path = (#Obj.t as 'a) Bed.format Guizmin_table.file_path

let with_rows ?header ?sep x ~f = Guizmin_table.with_rows (module Row) ?header ?sep x ~f
let load ?header ?sep x = Guizmin_table.load (module Row) (module Table) ?header ?sep x

let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
