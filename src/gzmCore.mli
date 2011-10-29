type path = string

type rule = {
  target : path ;
  recipe : script ;
  cleanup : script ;
  deps : rule list
}
and script = string list

type 'a target = private rule
type 'a factory = ?path:path -> unit -> 'a target

val path : 'a target -> string

val input : path -> 'a target
val factory : 
  recipe:script -> ?cleanup:script -> deps:rule list -> 'a factory



type 'a file = < format : 'a   ; kind : [`file] > target
type 'a dir  = < contents : 'a ; kind : [`dir]  > target 

val ( $ ) : 'a dir -> path -> 'b target


