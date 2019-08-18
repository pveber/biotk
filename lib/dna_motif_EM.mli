module Naive_MEME : sig
  module Simulation : sig
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }
    val make :
      Profile_matrix.DNA.t ->
      motif_probability:float ->
      n_sequences:int ->
      t
  end
end
