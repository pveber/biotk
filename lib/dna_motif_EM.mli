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

  type input = string array

  type param = {
    pi : float ;
    motif : float array array ;
    bg : float array ;
  }

  val infer :
    motif_length:int ->
    input ->
    (float array * param) list

  val demo :
    ?alpha:float ->
    ?motif_length:int ->
    unit -> unit
end
