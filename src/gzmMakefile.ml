open Batteries
open Printf

open GzmCore

module S = Set.Make(
  struct
    type t = rule
    let compare x y = compare x.target y.target
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
    (String.concat " " (List.map (fun r -> r.target) r.deps)) ;
  List.iter (fprintf oc "\t$(shell %s)\n") r.recipe ;
  output_string oc "\n"

let to_channel oc mk = 
  output_string oc "SHELL := /bin/bash\n" ;
  iter (rule_to_channel oc) mk
