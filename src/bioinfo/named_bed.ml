open Core.Std
open Guizmin
open MBSchema
open Printf

type tabular data = {
  chrom : string ;
  chromStart : int ;
  chromEnd : int ;
  name : string ;
}

type file' = Obj.t Guizmin_table.file
type file_path' = Obj.t Guizmin_table.file_path

type 'a file = (#Obj.t as 'a) Guizmin_table.file
type 'a file_path = (#Obj.t as 'a) Guizmin_table.file_path

include Guizmin_table.MakeOpen(Row)(Table)

let make ?(prefix = "seq_") bed =
  f1
    "guizmin.bioinfo.bed.named.make[r1]" 
    Param.([ string "prefix" prefix ])
    bed
    (fun env bed path ->
      let rename i { Bed.chrom ; chromStart ; chromEnd } =
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
      Bed.with_rows bed ~f:(fun xs ->
        Biocaml_stream.mapi xs ~f:rename |! save
      )
    )

let location_of_row { chrom ; chromStart ; chromEnd } = Location.make chrom chromStart chromEnd














