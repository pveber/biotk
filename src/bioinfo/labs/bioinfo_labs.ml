open Core.Std
module Stream = Biocaml_stream
open Stream.Infix
open Printf
open Guizmin
open Guizmin_bioinfo

let selection_of_bed (File bed) =
  In_channel.with_file bed ~f:(fun ic ->
    Bed.Row.stream_of_channel ic
    /@ Bed.location_of_row
    |! Biocaml.GenomeMap.Selection.of_stream
  )

let bed_intersection_filter ?(be_in_all = []) ?(not_in_any = []) bed =
  f3
    "prabi.benoit.utils.bed_intersection_filter[r3]" []
    (merge be_in_all)
    (merge not_in_any)
    bed
    (fun env be_in_all not_in_any (File bed) path ->
      let be_in_all = 
        let sels = List.map be_in_all ~f:selection_of_bed  in
        fun x -> List.for_all sels ~f:(Biocaml.GenomeMap.Selection.intersects x)
      in
      let not_in_any =
        let sels = List.map not_in_any ~f:selection_of_bed in
        fun x -> List.for_all sels ~f:(fun sel -> not (Biocaml.GenomeMap.Selection.intersects x sel))
      in
      (* let item_filter (chrom, st, ed, _) = *)
      (*   let loc = chrom, Biocaml.Range.make st ed in *)
      (*   be_in_all loc && not_in_any loc *)
      (* in *)
      let item_filter line =
        let fields = String.split (line : Biocaml_lines.item :> string) ~on:'\t' |! Array.of_list in
        let { Bed.chrom ; chromStart ; chromEnd } = Bed.Row.of_array fields in
        let loc = chrom, Biocaml.Range.make chromStart chromEnd in
        be_in_all loc && not_in_any loc
      in
      In_channel.with_file bed ~f:(fun ic ->
        (* Biocaml_stream.lines_of_channel ic *)
        (* |! Transform.stream_transformation (bed_transform_string_to_t ~filename:bed ()) *)
        (* |! Stream.filter ~f:item_filter *)
        (* |! Transform.stream_transformation (Bed.Transform.t_to_string ()) *)
        (* |! fun xs -> Out_channel.with_file path ~f:(Stream.lines_to_channel xs) *)
        Biocaml_lines.of_channel ic
        |! Stream.filter ~f:item_filter
        |! fun xs -> Out_channel.with_file path ~f:(Biocaml_lines.to_channel xs)
      )
    )

let bed_union beds =
  f1
    "guizmin.bioinfo.labs.bed_union[r1]" []
    (merge beds)
    (
      fun env beds path ->
        List.map beds ~f:selection_of_bed
        |> List.reduce ~f:Biocaml.GenomeMap.Selection.union
        |> Option.value_map ~default:(Stream.empty ()) ~f:Biocaml.GenomeMap.Selection.to_stream
        |> Stream.map ~f:Bed.row_of_location
        |> fun xs -> Out_channel.with_file path ~f:(fun ic -> Bed.Row.stream_to_channel ic xs)
    )
      
      




















