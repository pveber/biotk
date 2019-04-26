open Core
open CFStream
open Biocaml_base

module Item = struct
  type t = Gff.item

  let parse line =
    match Gff.gtf_item_of_line line with
    | Ok item -> item
    | Error (`Msg msg) -> failwith msg

  let unparse item =
    (Gff.line_of_item `two item :> string)

  let to_record = function
    | `Record r -> Some r
    | `Comment _ -> None
end

include Line_oriented.Make(Item)

module Record = struct
  type t = Gff.record
  let loc r = GLoc.{ chr = r.Gff.seqname ; lo = r.start_pos ; hi = r.stop_pos }
  let compare p q =
    match GLoc.compare (loc p) (loc q) with
    | 0 -> Option.compare String.compare p.feature q.feature
    | c -> c
end

module Annotation = struct
  type t = {
    gene_id_label : string ;
    transcript_id_label : string ;
    items : Gff.record list String.Table.t ;
  }

  let of_items ?(gene_id_label = "gene_id") ?(transcript_id_label = "transcript_id") items =
    let items =
      CFStream.Stream.of_list items
      |> CFStream.Stream.filter_map ~f:(function
          | `Comment _ -> None
          | `Record r ->
            match List.Assoc.find r.Biocaml_base.Gff.attributes gene_id_label ~equal:String.equal with
            | Some (id :: _) -> Some (id, r)
            | Some []
            | None -> None
        )
      |> Biocaml_unix.Accu.relation
      |> CFStream.Stream.map ~f:(fun (x, y) -> x, List.rev y)
      |> CFStream.Stream.to_list
      |> String.Table.of_alist_exn
    in
    { gene_id_label ; transcript_id_label ; items }

  let%test_unit "Annotation.of_items tail rec" =
    List.init 1_000_000 ~f:(fun _ -> `Comment "")
    |> of_items
    |> ignore

  let strand_of_entry items =
    let strands =
      List.map items ~f:(fun i -> i.Gff.strand)
      |> List.dedup_and_sort ~compare:Poly.compare
    in
    match strands with
    | [] -> raise (Invalid_argument "strand_of_entry")
    | [ s ] -> Ok s
    | _ -> Or_error.error_string "more than one strand in entry"

  let exons_of_entry items =
    List.filter_map items ~f:(fun r ->
        match r.Gff.feature with
        | Some "exon" -> Some r
        | _ -> None
      )

  let sort_by_attribute ~attr_label items =
    Stream.of_list items
    |> Stream.filter_map ~f:(fun r ->
        match List.Assoc.find r.Biocaml_base.Gff.attributes attr_label ~equal:String.equal with
          | Some (id :: _) -> Some (id, r)
          | Some []
          | None -> None
      )
    |> Biocaml_unix.Accu.relation
    |> Stream.to_list

  let gene_of_entry ~id ~transcript_id_label items =
    match exons_of_entry items with
    | [] -> Ok None
    | exons ->
      let open Or_error.Monad_infix in
      strand_of_entry exons |> Or_error.tag ~tag:id >>= function
      | `Unknown | `Not_stranded -> Or_error.error_string "no defined strand"
      | `Plus | `Minus as strand ->
        let transcripts =
          sort_by_attribute ~attr_label:transcript_id_label exons
          |> List.map ~f:(fun (s, items) -> s, List.map ~f:Record.loc items)
        in
        Gene.make ~strand ~id transcripts
        |> Or_error.map ~f:Option.some

  let genes annot =
    let r = String.Table.create () in
    let errors =
      String.Table.fold annot.items ~init:[] ~f:(fun ~key:id ~data:items errors ->
          match gene_of_entry ~id ~transcript_id_label:annot.transcript_id_label items with
          | Ok (Some g) ->
            String.Table.set r ~key:id ~data:g ;
            errors
          | Ok None -> errors
          | Error e -> (id, e) :: errors
        )
    in
    r, errors

  let utr5' annot =
    String.Table.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Gff.feature with
            | Some "5UTR" -> Some r
            | _ -> None
          )
      )

  let utr3' annot =
    String.Table.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Gff.feature with
            | Some "3UTR" -> Some r
            | _ -> None
          )
      )
end
