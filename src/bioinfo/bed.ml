open Core.Std
open Guizmin
open MBSchema

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int 
}
type file' = Obj.t Guizmin_table.file
type file_path' = Obj.t Guizmin_table.file_path

type 'a file = (#Obj.t as 'a) Guizmin_table.file
type 'a file_path = (#Obj.t as 'a) Guizmin_table.file_path

include Guizmin_table.MakeOpen(Row)(Table)

let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd

let row_of_location (chrom, { Biocaml_range.lo = chromStart ; hi = chromEnd }) = 
  { chrom ; chromStart ; chromEnd }

let of_locations locs =
  f1
    "guizmin.bioinfo.bed.minimal.of_locations[r1]" []
    locs
    (
      fun env locs path ->
        Out_channel.with_file path ~f:(fun oc ->
          List.map locs row_of_location
                                          |> Stream.of_list
                                          |> Row.stream_to_channel oc
        )
    )


type track

















