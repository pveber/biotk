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
