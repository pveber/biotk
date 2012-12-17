open Guizmin
open MBSchema

type table genomic_coordinate = {
  loc : Location
}

include Guizmin_table.NEWAPI.Impl
