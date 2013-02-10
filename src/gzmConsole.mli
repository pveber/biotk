open GzmCore

val cache_strata : base_directory -> (string * Unix.tm * Int64.t * Int64.t) list
val cache_stratum : base_directory -> int -> (string * Unix.tm * Int64.t) option
val cache_stratum_hum : base_directory -> int -> (string * string * float) option
val clear_cache : base_directory -> int -> unit




















