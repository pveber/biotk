open Core

type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp, show]

module Parser = struct
  open Angstrom

  let is_not_eol = function
    | '\n' | '\r' -> false
    | _ -> true

  let eolf = end_of_line <|> end_of_input

  let comment_line =
    choice [ char ';' ; char '#' ] >>= fun comment_char ->
    take_while is_not_eol >>= fun comment_msg ->
    eolf *>
    return (comment_char, comment_msg)

  let header = many comment_line

  let description_line =
    char '>' *> take_while is_not_eol <* eolf

  let sequence_line =
    peek_char_fail >>= (function
        | '>' -> fail "Expected sequence line, not description"
        | _ -> take_while is_not_eol
      ) <* eolf

  let item =
    description_line >>= fun description ->
    many sequence_line >>= fun seqs ->
    let sequence = String.concat seqs in
    return { description ; sequence }

  let fasta =
    let p =
      header >>= fun header ->
      many item >>= fun items ->
      end_of_input >>| fun () ->
      List.map ~f:snd header, items
    in
    p <?> "fasta"
end

let from_string s = Angstrom.(parse_string ~consume:All) Parser.fasta s

let%expect_test "parse FASTA without header" =
  let fa = from_string ";qsdf\n>s1\nAA\n>s2\nA\n" in
  print_endline @@ [%show: (string list * item list, string) result] fa ;
  [%expect {||}]

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      let Angstrom.Buffered.{ buf ; off ; len }, res = Angstrom_unix.parse Parser.fasta ic in
      match res with
      | Ok r -> Ok r
      | Error _ ->
        let snippet = Bigstringaf.substring buf ~off ~len:(Int.min 30 len) in
        let msg = sprintf "Failed to parse: %s" snippet in
        Error msg
    )

let from_file_exn fn =
  from_file fn
  |> Result.ok_or_failwith

let sequences_from_file_exn fn =
  match from_file fn with
  | Ok (_, items) -> List.map items ~f:(fun i -> i.sequence)
  | Error msg -> failwith msg

let to_file fn items =
  Out_channel.with_file fn ~f:(fun oc ->
      List.iter items ~f:(fun it ->
          fprintf oc ">%s\n%s\n" it.description it.sequence
        )
    )
