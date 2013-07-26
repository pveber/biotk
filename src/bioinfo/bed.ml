open Core.Std
open Guizmin
open MBSchema

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int 
}

type 'a format constraint 'a = #Obj.t

type file' = Obj.t format Guizmin_table.file
type file_path' = Obj.t format Guizmin_table.file_path

type 'a file = (#Obj.t as 'a) format Guizmin_table.file
type 'a file_path = (#Obj.t as 'a) format Guizmin_table.file_path

let with_rows ?header ?sep x ~f = Guizmin_table.with_rows (module Row) ?header ?sep x ~f
let with_rows_obj ?header ?sep x ~f = Guizmin_table.with_rows (module Row) ?header ?sep x ~f:(fun xs ->
  f (Biocaml_stream.map xs ~f:Obj.of_row)
)
let load ?header ?sep x = Guizmin_table.load (module Row) (module Table) ?header ?sep x

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

















