open Batteries

type path = string

type rule = {
  target : path ;
  recipe : script ;
  cleanup : script ;
  deps : rule list
}
and script = string list

type 'a target = rule
type 'a factory = ?path:path -> unit -> 'a target

let path t = t.target

let factory ~recipe ?(cleanup = []) ~deps ?path () = {
  target = (
    match path with 
	None -> assert false
      | Some p -> p
  ) ;
  recipe ;
  cleanup ;
  deps
}

let input path = factory ~recipe:[] ~deps:[] ~path ()

type 'a file = < format : 'a   ; kind : [`file] > target
type 'a dir  = < contents : 'a ; kind : [`dir]  > target 

let ( $ ) dir path = 
  factory ~recipe:[] ~deps:[ dir ] ~path:(dir.target ^ "/" ^ path) ()

    
