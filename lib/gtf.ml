open Core
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
end

module Annotation = struct
  type t = Gff.record list String.Table.t

  let of_items ~id_label items =
    Stream.of_list items
    |> CFStream.Stream.filter_map ~f:(function
        | `Comment _ -> None
        | `Record r ->
          match List.Assoc.find r.Biocaml_base.Gff.attributes id_label ~equal:String.equal with
          | Some (id :: _) -> Some (id, r)
          | Some []
          | None -> None
      )
    |> Biocaml_unix.Accu.relation
    |> CFStream.Stream.to_list
    |> String.Table.of_alist_exn

  let strand_of_entry items =
    let strands =
      List.map items ~f:(fun i -> i.Gff.strand)
      |> List.dedup_and_sort ~compare:Poly.compare
    in
    match strands with
    | [] -> raise (Invalid_argument "strand_of_entry")
    | [ s ] -> Ok s
    | _ -> Or_error.error_string "more than one strand in entry"

  let gene_of_entry items =
    let exons =
      List.filter_map items ~f:(fun r ->
          match r.Gff.feature with
          | Some "exon" -> Some r
          | _ -> None
        )
    in
    match exons with
    | [] -> Ok None
    | _ ->
      let open Or_error.Monad_infix in
      strand_of_entry exons >>= function
      | `Unknown | `Not_stranded -> Or_error.error_string "no defined strand"
      | `Plus | `Minus as strand ->
        Gene.make ~strand ~exons:(List.map exons ~f:Record.loc)
        |> Or_error.map ~f:Option.some

  let genes annot =
    let exception E of Error.t in
    try
      Ok (
        String.Table.to_alist annot
        |> List.filter_map ~f:(fun (id, items) ->
            match gene_of_entry items with
            | Ok (Some g) -> Some (id, g)
            | Ok None -> None
            | Error e -> raise (E e)
          )
      )
    with E msg -> Error msg
end
