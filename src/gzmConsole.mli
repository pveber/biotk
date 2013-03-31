open GzmCore

type cache_selection
val cache_selection : 
  ?used_less_than:(int * int) -> 
  ?req_less_than:(int * int) -> 
  ?bigger_than:float -> (** size in GB *)
  base_directory -> cache_selection

(** human-readable size in GB *)
val size_of_selection : cache_selection -> float

val clear_selection : cache_selection -> unit




















