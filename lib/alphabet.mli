(** Alphabet representation

    This module provides a functor to generate modules that implement
    an alphabet datatype. Internally, elements of the alphabet are
    represented as simple [char] values but this is hidden by the
    module signature to make sure we only manipulate valid characters
    of a given alphabet. *)

module type S = sig
  type t
  val all : t list
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(_ : sig val letters : string end) : S
