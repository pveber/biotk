open Core_kernel.Std

type 'a item = {
  header : string ;
  sequence : 'a
}

type parsing_error = string * int

val read : in_channel -> (string item,  parsing_error) Result.t Stream.t
val read_ints : in_channel -> (int list item, parsing_error) Result.t Stream.t

(* val to_channel : out_channel -> item Stream.t -> unit *)
