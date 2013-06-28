open Guizmin
open Guizmin_bioinfo

val bed_intersection_filter : 
  ?be_in_all:'a Bed.file list ->
  ?not_in_any:'c Bed.file list ->
  'd Bed.file -> 'd Bed.file

val bed_union : 'a Bed.file list -> Bed.Minimal.file

