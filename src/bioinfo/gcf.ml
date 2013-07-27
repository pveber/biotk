open Core.Std
open Biocaml_stream.Infix
open Guizmin
open MBSchema

module X = struct
  type tabular data = {
    loc : Location
  }
end

include X
include Guizmin_table.Make(X)

let bed_row_of_genomic_coordinate { loc = (chrom, { Biocaml.Range.lo ; hi } ) } =
  {
    Bed.Basic.chrom ;
    chromStart = lo ;
    chromEnd = hi 
  }

let to_bed gcf =
  f1
    "guizmin.bioinfo.gcf.to_bed[r1]" []
    gcf
    (fun env (File gcf) path ->
      In_channel.with_file gcf ~f:(fun ic ->
        Row.stream_of_channel ic
        /@ bed_row_of_genomic_coordinate
        |> fun x -> Out_channel.with_file path ~f:(fun oc -> 
          Bed.Basic.Row.stream_to_channel oc x
        )
      ))




















