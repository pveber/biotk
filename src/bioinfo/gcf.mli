(** GCF = Genomic coordinates format loosely defined as a TSV format
    whose first column represents a location with the syntax <chr>:<start>-<end>
*)

open Guizmin
open MBSchema

type tabular data = {
  loc : Location
}

include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty

val to_bed : file -> Bed.Basic.file



















