(** GCF = Genomic coordinates format loosely defined as a TSV format
    whose first column represents a location with the syntax <chr>:<start>-<end>
*)

open Guizmin
open MBSchema

type table genomic_coordinate = {
  loc : Location
}

include Guizmin_table.NEWAPI.S
