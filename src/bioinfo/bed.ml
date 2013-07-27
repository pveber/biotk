open Core.Std
open Guizmin
open MBSchema

module Basic = struct
  module X = struct
    type tabular data = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int 
    }
  end
  include X
  include Guizmin_table.Make(X)

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
  module X = struct
    type tabular data = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
    }
  end
  include X
  include Guizmin_table.Make(X)

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
          Biocaml_stream.mapi xs ~f:rename |! save
        )
      )

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

module Stranded = struct
  module X = struct
    type tabular data = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : float ;
      strand : [`sense "+" | `antisense "-"] ;
    }
  end
  include X
  include Guizmin_table.Make(X)

  let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd
end

type 'a file = 'a Basic.file'
type 'a named_file = 'a Named.file'
type 'a stranded_file = 'a Stranded.file'


type track



















