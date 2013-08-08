open Core.Std

open GzmUtils

type path = string list
type item =
  | Item : 'a Guizmin.pipeline * string * path -> item
let item ?(descr = "") path pipeline = Item (pipeline, descr, path)

type t = item list

let find_duplicate_paths repo =
  List.(
    map repo ~f:(function Item (_,_,x) -> x)
    |! find_a_dup
  )

let fail_on_duplicate_paths items = match find_duplicate_paths items with
| None -> ()
| Some p ->
    let path = String.concat ~sep:"/" p in
    failwithf "Path %s is present several times in the repo!" path ()

let create ?(np = 0) ?(wipeout = false) ~base ~repo_base items =
  fail_on_duplicate_paths items ;
  if not (Sys.file_exists repo_base) then sh "mkdir -p %s" repo_base ;
  if wipeout then sh "rm -rf %s/*" repo_base ;
  List.iter items ~f:(
    function Item (pipeline,_,rel_path)  ->
      let abs_path = repo_base ^ "/" ^ (String.concat ~sep:"/" rel_path) in
      if np > 0 then Guizmin.build ~base ~np pipeline ;
      bash [
        sp "mkdir -p %s" (Filename.dirname abs_path) ;
        sp "ln -s `readlink -f %s` %s" (Guizmin.path ~base pipeline) abs_path
      ]
  )

















