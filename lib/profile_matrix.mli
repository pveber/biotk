open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw :
    ?palette:Gg.Color.t array ->
    t ->
    Croquis.t
  val draw_profile :
    ?palette:Gg.Color.t array ->
    float array ->
    Croquis.t
  val entropy : t -> float array
end

module Make(_ : Alphabet.S) : S

module DNA : sig
  include S
  val reverse_complement : t -> t
end

module Protein : sig
  include S
  val dayhoff_palette : Gg.color array
end


val random_profile :
  (module Alphabet.S) ->
  float ->
  Gsl.Rng.t ->
  float array
