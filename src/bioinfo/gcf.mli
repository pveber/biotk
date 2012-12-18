(** GCF = Genomic coordinates format loosely defined as a TSV format
    whose first column represents a location with the syntax <chr>:<start>-<end>
*)

open Guizmin
open MBSchema

type table genomic_coordinate = {
  loc : Location
}

type file = Genomic_coordinate.s Guizmin_table.NEWAPI.file

val to_bed : file -> Bed.minimal_file










