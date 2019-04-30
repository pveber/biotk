open Core_kernel

type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp]

module Parser = struct
  open Angstrom

  let is_not_eol = function
    | '\n' | '\r' -> false
    | _ -> true

  let comment_line =
    choice [ char ';' ; char '#' ] >>= fun comment_char ->
    take_while is_not_eol >>| fun comment_msg ->
    (comment_char, comment_msg)

  let header = sep_by end_of_line comment_line

  let description_line =
    char '>' *> take_while is_not_eol

  let sequence_line =
    peek_char_fail >>= (function
        | '>' -> fail "Expected sequence line, not description"
        | _ -> take_while is_not_eol
      )

  let item =
    description_line <* end_of_line >>= fun description ->
    many (sequence_line <* end_of_line) >>| fun seqs ->
    let sequence = String.concat seqs in
    { description ; sequence }

  let fasta =
    header >>= fun header ->
    many item >>| fun items ->
    List.map ~f:snd header, items
end

let from_file fn =
  In_channel.with_file fn ~f:(fun ic ->
      let _, res = Angstrom_unix.parse Parser.fasta ic in
      res
    )
