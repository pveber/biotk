open Core

type item = {
  description : string ;
  sequence : string ;
  comment : string ;
  quality : string ;
}
[@@deriving sexp]

type parsing_error = [
  | `Malformed_line of int * string
  | `Unexpected_EOF
]
[@@deriving sexp]

let or_eof = function
  | None -> Error `Unexpected_EOF
  | Some x -> Ok x

let read_annotation prefix line_number ic =
  let open Let_syntax.Result in
  let* c = In_channel.input_char ic |> or_eof in
  if Char.equal c prefix then
    In_channel.input_line ic |> or_eof
  else Error (`Malformed_line (line_number, sprintf "Expected character '%c' at beginning of line" prefix))

let read_sequence ic =
  In_channel.input_line ic |> or_eof

let read_item line_number ic =
  let open Let_syntax.Result in
  let* description = read_annotation '@' line_number ic in
  let* sequence = read_sequence ic in
  let* comment = read_annotation '+' (line_number + 2) ic in
  let+ quality = read_sequence ic in
  { description ; sequence ; comment ; quality }

let fold_file fn ~init ~f =
  In_channel.with_file fn ~f:(fun ic ->
      let rec loop ~lno acc =
        let open Let_syntax.Result in
        let* item = read_item lno ic in
        let* acc' = f acc item in
        loop ~lno:(lno + 4) acc'
      in
      loop ~lno:0 init
    )

module Stats = struct
  type t = {
    nb_reads : int ;
  }
  let of_file fq =
    let open Let_syntax.Result in
    let+ nb_reads = fold_file fq ~init:0 ~f:(fun n _ -> Ok (n + 1)) in
    { nb_reads }
end
