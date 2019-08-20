include Alphabet.S

val a : t
val c : t
val g : t
val t : t

val complement : t -> t

module Vector : Alphabet.Vector
module Sequence : Alphabet.Sequence

val random : float Vector.t -> t
