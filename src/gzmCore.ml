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

type 'a target = protocol
type 'a file = < format : 'a ; kind : [`file] > target
type 'a dir  = < contents : 'a ; kind : [`directory] > target

let input p = Input p
let step ?(body = []) ?(cleanup = []) ?(deps = []) target = 
  Step { target ; body ; cleanup ; deps }
let select p t = Select (p,t)

let rec path = function
    Input p -> p
  | Step s -> s.target 
  | Select (p, t) -> (path t) ^ "/" ^ p

let ( ++ ) deps f = (f :> protocol) :: deps
let ( ++* ) deps files = (files :> protocol list) @ deps

