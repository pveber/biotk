open Core.Std
open Guizmin
open MBSchema

module Minimal = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int 
  }
  type file = Obj.t Guizmin_table.file
  include Guizmin_table.Make(Row)(Table)

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
end

module Named = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  type file = Obj.t Guizmin_table.file

  include Guizmin_table.Make(Row)(Table)

  let make ?(prefix = "seq_") bed =
    f1
      "guizmin.bioinfo.bed.named.make[r1]" 
      Param.([ string "prefix" prefix ])
      bed
      (fun env bed path ->
        let rename i { Minimal.chrom ; chromStart ; chromEnd } =
          {
            chrom ; chromStart ; chromEnd ;
            name = sprintf "%s_%d" prefix i
          }
        in
        let save xs = 
          Out_channel.with_file path ~f:(fun oc -> 
            Row.stream_to_channel oc xs
          )
        in
        Minimal.with_rows bed ~f:(fun xs ->
          Biocaml_stream.mapi xs ~f:rename |! save
        )
      )

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

module Stranded = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : [`sense "+" | `antisense "-"] ;
  }
  type file = Obj.t Guizmin_table.file
  include Guizmin_table.Make(Row)(Table)
  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

type 'a file = (#Minimal.Obj.t as 'a) Guizmin_table.file
type 'a named_file = (#Named.Obj.t as 'a) Guizmin_table.file
type 'a stranded_file = (#Stranded.Obj.t as 'a) Guizmin_table.file

type track

















