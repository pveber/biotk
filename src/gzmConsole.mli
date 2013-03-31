open GzmCore

val cache_strata : base_directory -> (string * Unix.tm * Int64.t * Int64.t) list
val cache_stratum : base_directory -> int -> (string * Unix.tm * Int64.t) option
val cache_stratum_hum : base_directory -> int -> (string * string * float) option
val clear_cache : base_directory -> int -> unit

type cache_selection
val cache_selection : 
  ?used_less_than:(int * int) -> 
  ?req_less_than:(int * int) -> 
  ?bigger_than:int ->
  base_directory -> cache_selection




















