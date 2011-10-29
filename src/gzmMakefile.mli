open Batteries

type build_rule = {
  target : string ;
  deps : string list ;
  cmds : string list
}

include Set.S with type elt = build_rule

val concat : t list -> t
val to_channel : unit IO.output -> t -> unit
