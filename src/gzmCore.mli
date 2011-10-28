type protocol = 
    Input of path
  | Step of step
  | Select of path * protocol
and step = {
  target : string ;
  body : script ;
  cleanup : script ;
  deps : protocol list
}
and path = string
and script = string list

type 'a target = private protocol
type 'a file = < format : 'a ; kind : [`file] > target
type 'a dir  = < contents : 'a ; kind : [`directory] > target

val input : path -> 'a target
val step : ?body:script -> ?cleanup:script -> ?deps:protocol list -> string -> 'a target
val select : path -> 'a dir -> 'a target


val path : 'a file -> string
val ( ++ ) : protocol list -> 'a target -> protocol list
val ( ++* ) : protocol list -> 'a target list -> protocol list



val compile : 'a target -> GzmMakefile.t
val sp : ('a, unit, string) format -> 'a
