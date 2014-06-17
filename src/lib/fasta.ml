open Core_kernel.Std
open CFStream

type item = {
  comment : string ;
  seq : string
}

let chopl s = String.(sub s ~pos:1 ~len:(length s - 1))

let is_comment x = String.is_prefix x ~prefix:">"

let read_sequence lines =
  let open Or_error in
  match Stream.next lines with
  | None -> error_string "Fasta.read: incorrect syntax"
  | Some l ->
    let rec loop accu =
      match Stream.next lines with
      | None -> accu, lines
      | Some l ->
        if is_comment l then accu, Stream.append (Stream.singleton l) lines
        else loop (accu ^ l)
    in
    if is_comment l then error_string "Fasta.read: incorrect syntax"
    else return (loop l)

let read_item lines =
  let open Or_error in
  match Stream.next lines with
  | None -> None
  | Some l ->
    if String.is_prefix l ~prefix:">" then
      let comment = chopl l in
      match read_sequence lines with
      | Result.Ok (seq, lines) ->
        Some (return { comment ; seq }, lines)
      | Result.Error e -> Some (Result.Error e, Stream.empty ())
    else
      Some (error_string "Fasta.read_item: incorrect syntax", Stream.empty ())

let of_channel ic =
  Biocaml_lines.of_channel ic
  |> Stream.map ~f:(fun x -> (x : Biocaml_line.t :> string))
  |> Stream.unfold ~f:read_item
