module Profile_matrix : sig
  type t = private float array array
  val length : t -> int
  val random : ?alpha:float -> int -> t
  val simulate_sequence : t -> string
  val composition : t -> float array
end

module Naive_MEME : sig
  module Simulation : sig
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }
    val make :
      Profile_matrix.t ->
      motif_probability:float ->
      n_sequences:int ->
      t
  end
end
