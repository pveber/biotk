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

val fold_file :
  string ->
  init:'a ->
  f:('a -> item -> ('a, 'b) result) ->
  ('a, [> parsing_error ] as 'b) result

module Stats : sig
  type t = {
    nb_reads : int ;
  }
  val of_file : string -> (t, [> parsing_error]) result
end
