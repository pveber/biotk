open Batteries

include Set.S with type elt = GzmCore.rule

val concat : t list -> t
val to_channel : unit IO.output -> t -> unit
