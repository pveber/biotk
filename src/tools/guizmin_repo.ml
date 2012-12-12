open Guizmin.Utils

type path = string list
type item = 
  | Item : 'a Guizmin.pipeline * string * path -> item
let item ~descr path pipeline = Item (pipeline, descr, path)

type t = item list

let create ?(force = false) items base_path =
  sh "rm -rf %s" base_path ;
  sh "mkdir -p %s" base_path ;
  List.iter
    (function Item (pipeline,_,rel_path)  ->
      let abs_path = base_path ^ "/" ^ (String.concat "/" rel_path) in
      if force then ignore (Guizmin.eval pipeline) ;
      sh "mkdir -p %s" (Filename.dirname abs_path) ;
      sh "ln -s %s %s" (Guizmin.path pipeline) abs_path)
    items
        




















