open Core.Std
open CFStream
open Guizmin
open MBSchema

module Basic = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int
  }
  include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header)

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
  include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header)

  let make ?(prefix = "seq_") bed =
    f1
      "guizmin.bioinfo.bed.named.make[r1]"
      Param.([ string "prefix" prefix ])
      bed
      (fun env bed path ->
        let rename i { Basic.chrom ; chromStart ; chromEnd } =
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
        Basic.with_rows bed ~f:(fun xs ->
          Stream.mapi xs ~f:rename |! save
        )
      )

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

module Scored = struct
  type tabular data = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
  }
  include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header)

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd

  let filter ?less_than ?more_than bed =
    f1
      "guizmin.bioinfo.bed.scored.filter[r2]"
      Param.([opt float "less_than" less_than ; opt float "more_than" more_than])
      bed
      (
        fun env (File bed) path ->
          let parse =
            Biocaml_line.split ~on:'\t'
            |- Array.of_list
            |- Row.of_array
          in
          let true_ _ = true in
          let pred =
            let less_than = Option.value_map less_than ~default:true_ ~f:(fun ub -> fun x -> x <= ub)
            and more_than = Option.value_map more_than ~default:true_ ~f:(fun lb -> fun x -> lb <= x) in
            fun x -> less_than x && more_than x
          in
          In_channel.with_file bed ~f:(fun ic ->
            Biocaml_lines.of_channel ic
            |> Stream.map ~f:(fun l -> l, parse l)
            |> Stream.filter_map ~f:(fun (l,r) -> if pred r.score then Some l else None)
            |> fun xs -> Out_channel.with_file path ~f:(Biocaml_lines.to_channel xs)
          )
      )
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
  include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.No_header)

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

type 'a file = 'a Basic.file'
type 'a named_file = 'a Named.file'
type 'a scored_file = 'a Scored.file'
type 'a stranded_file = 'a Stranded.file'


type track



















