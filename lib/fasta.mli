type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp]


val from_string :
  string ->
  (header * item list, string) result

val from_file :
  string ->
  (header * item list, string) result
