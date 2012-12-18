open Core.Std
open Biocaml_stream.Infix
open Guizmin
open MBSchema

type table genomic_coordinate = {
  loc : Location
}

type file = Genomic_coordinate.s Guizmin_table.NEWAPI.file

let bed_row_of_genomic_coordinate { Genomic_coordinate.loc = (chrom, { Biocaml.Range.lo ; hi } ) } =
  {
    Bed.Minimal.chrom ;
    chromStart = lo ;
    chromEnd = hi 
  }

let to_bed gcf =
  f1
    ("guizmin.bioinfo.gcf.to_bed[r1]", [])
    (fun env (File gcf) path ->
      In_channel.with_file gcf ~f:(fun ic ->
        Genomic_coordinate.stream_of_channel ic
        /@ bed_row_of_genomic_coordinate
        |! fun x -> Out_channel.with_file path ~f:(fun oc -> 
          Bed.Minimal.stream_to_channel oc x
        )
      ))
    gcf




















