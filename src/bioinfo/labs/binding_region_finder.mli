open Guizmin
open Guizmin_bioinfo.MBSchema

type binding_region = {
  chrom : string ;
  summits : int list ;
  center : int ;
  convex_hull : Location.t
}

val centered_location_of_binding_region : radius:int -> binding_region -> Location.t

val of_macs_peaks : Guizmin_bioinfo.Macs.Wo_control.Peak.file list -> binding_region list pipeline










