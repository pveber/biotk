open Printf
open Guizmin

let eval x = Guizmin.(eval ~base:(default_base_directory ()) x)

let p i s = 
  v0 
    "guizmin.test.1" 
    Param.([ int "i" i ; string "s" s ])
    (fun env -> i = int_of_string s)

let () =
  printf "%b %b\n"
    (eval (p 0 "1"))
    (eval (p 1 "1"))










