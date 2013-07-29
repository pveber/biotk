open GzmCore

type cache_selection

val cache_selection : 
  ?used_less_than:(int * int * int) -> 
  ?req_less_than:(int * int * int) -> 
  ?bigger_than:float -> (** size in GB *)
  base_directory -> cache_selection
(** [cache_selection ~used_less_than:(k,m,n) ~req_less_than:(k',m',n')
    ~bigger_than:s] finds files in cache than were used [k] times or
    less in the period ranging from [m] days ago to [n] days ago, and
    were (directly) requested [k'] times or less in the period from
    [m'] days ago to [n'] days ago, and that are smaller than [s]
    GB. *)

val size_of_selection : cache_selection -> float
(** human-readable size in GB *)

val clear_selection : cache_selection -> unit




















