module type S = sig
  type composition
  type sequence
  type t = private composition array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val random : ?alpha:float -> int -> t
  val simulate_sequence : t -> sequence
  val composition : t -> composition
  val draw : t -> Croquis.Picture.t
end

module type Alphabet = sig
  include Alphabet.S
  module Vector : Alphabet.Vector
  module Sequence : Alphabet.Sequence
  val random : float Vector.t -> t    
end

module Make(A : Alphabet) : S with type composition := float A.Vector.t
                               and type sequence := A.Sequence.t
module DNA : sig
  include (module type of Make(Nucleotide))
  val reverse_complement : t -> t
end
