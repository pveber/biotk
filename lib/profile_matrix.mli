module type S = sig
  type t = private float array array
  val length : t -> int
  val random : ?alpha:float -> int -> t
  val simulate_sequence : t -> string
  val composition : t -> float array
end

module type Alphabet = sig
  type t
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) : S

module DNA : S
