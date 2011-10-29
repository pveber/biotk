open Batteries
open Printf

type build_rule = {
  target : string ;
  deps : string list ;
  cmds : string list
}

module S = Set.Make(struct
		      type t = build_rule
		      let compare = compare
		    end)

include S

let concat l = 
  List.fold_left 
    (fun accu x -> union x accu)
    empty
    l

let rule_to_channel oc r = 
  fprintf oc "%s: %s\n" 
    r.target
    (String.concat " " r.deps) ;
  fprintf oc "\t%s" (String.concat " && \\\n\t" r.cmds) ;
  output_string oc "\n\n"

let to_channel oc mk = 
  output_string oc "SHELL := /bin/bash\n\n" ;
  output_string oc "all:\n\n" ;
  iter (rule_to_channel oc) mk
