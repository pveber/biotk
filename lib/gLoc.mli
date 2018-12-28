type t = {
  chr : string ;
  lo : int ;
  hi : int ;
} [@@deriving compare, sexp]

val of_string : string -> (t, [> `Parse_error]) result
val of_string_exn : string -> t
val to_string : t -> string

val range : t -> Range.t

val strictly_before : t -> t -> bool
val intersects : t -> t -> bool
