open GzmUtils

type path = string list
type item = 
  | Item : 'a Guizmin.pipeline * string * path -> item
let item ?(descr = "") path pipeline = Item (pipeline, descr, path)

type t = item list

let create ?(np = 0) ~base ~repo_base items =
  sh "rm -rf %s" repo_base ;
  sh "mkdir -p %s" repo_base ;
  List.iter
    (function Item (pipeline,_,rel_path)  ->
      let abs_path = repo_base ^ "/" ^ (String.concat "/" rel_path) in
      if np > 0 then Guizmin.build ~base ~np pipeline ;
      bash [
        sp "mkdir -p %s" (Filename.dirname abs_path) ;
        sp "ln -s `readlink -f %s` %s" (Guizmin.path ~base pipeline) abs_path
      ])
    items

















