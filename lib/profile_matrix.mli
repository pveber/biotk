open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw : t -> Croquis.t
  val entropy : t -> float array
end

module Make(A : Alphabet.S) : S

module DNA : sig
  include S
  val reverse_complement : t -> t
end
