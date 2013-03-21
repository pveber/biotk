open Printf
open Guizmin

let eval x = Guizmin.(eval ~base:(default_base_directory ()) x)

let f i _ =
  printf "%d\n" i ;
  i

let rec p i = match i with
| 0 | 1 | 2 -> v0 "guizmin.test.1" Param.([ int "i" i ]) (f i)
| _ -> 
    let p0 = p (i / 2) and p1 = p (i / 2 + 1) in
    v2 "guizmin.test.1" Param.([ int "i" i ]) p0 p1 (fun _ _ -> f i)


let _ = eval (p 18)










