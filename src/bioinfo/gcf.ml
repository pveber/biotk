open Core.Std
open Biocaml_stream.Infix
open Guizmin
open MBSchema

type tabular data = {
  loc : Location
}

type file = Row.t Guizmin_table.file

let bed_row_of_genomic_coordinate { loc = (chrom, { Biocaml.Range.lo ; hi } ) } =
  {
    Bed.Minimal.chrom ;
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
        |! fun x -> Out_channel.with_file path ~f:(fun oc -> 
          Bed.Minimal.Row.stream_to_channel oc x
        )
      ))




















