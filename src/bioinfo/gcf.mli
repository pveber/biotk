(** GCF = Genomic coordinates format loosely defined as a TSV format
    whose first column represents a location with the syntax <chr>:<start>-<end>
*)

open Guizmin
open MBSchema

type tabular data = {
  loc : Location
}

type file = Row.t Guizmin_table.file

val to_bed : file -> 'a Bed.file



















