type header = string list

type item = {
  description : string;
  sequence : string;
}
[@@ deriving sexp]

val from_file :
  string ->
  (header * item list, string) result
