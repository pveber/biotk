open Printf

type path = string
type param = string * [`int of int | `string of string | `float of float | `bool of bool ]
type id = string * param list

let string id v = id, `string v
let int id v = id, `int v
let float id v = id, `float v
let bool id b = id, `bool b


let mkdir s = 
  if not (Sys.file_exists s) then 
    Unix.mkdir s 0o755
  else
    assert (Sys.is_directory s)

let home_dir    = Sys.getcwd ()
let guizmin_dir = home_dir ^ "/_guizmin"
let cache_dir = guizmin_dir ^ "/cache"
let build_dir = guizmin_dir ^ "/_build"
let tmp_dir = guizmin_dir ^ "/tmp"

let _ = 
  mkdir guizmin_dir ;
  mkdir cache_dir ;
  mkdir build_dir ;
  mkdir tmp_dir

let hash x = x # hash

let path x = 
  cache_dir ^ "/" ^ (hash x)

let tmp_path x = 
  tmp_dir ^ "/" ^ (hash x)

let touch fn =
  ignore (Sys.command ("touch " ^ fn))

class dep id kind deps = 
object (self)
  method deps : dep list = deps

  method hash = 
    let content = id, kind, List.map (fun x -> x#hash) deps in
    Digest.to_hex (Digest.string (Marshal.to_string content []))

  method path = 
    cache_dir ^ "/" ^ (hash self)

  method tmp_path = 
    tmp_dir ^ "/" ^ (hash self)

  method clean = 
    Sys.remove self # path

  method built =
    Sys.file_exists self#path
end

let as_dep x = (x :> dep)

class type ['a] pipeline = object
  inherit dep
  method eval : 'a
end

let eval x = x # eval


let load_value path = 
  let ic = open_in path in 
  let r = input_value ic in
  close_in r ; r

let save_value v path = 
  let oc = open_out path in 
  output_value oc v ;
  close_out oc 

let value id f deps = 
object (self)
  inherit dep id `value deps
  method eval = 
    let dest = self#path
    and tmp = self#tmp_path in
    if not self#built then (
      let r = f () in
      save_value r tmp ;
      Sys.rename tmp dest ;
      r
    )
    else (
      touch dest ;
      load_value dest
    )
end

let v0 id f = value id f []

let v1 id f x = 
  value id (fun () -> f x#eval) [ as_dep x ]

let v2 id f x y = 
  value id (fun () -> f x#eval y#eval) [ as_dep x ; as_dep y ]

type 'a file = File of path
type 'a dir  = Dir of path

let path kind cons id f children =
object (self)
  inherit dep id kind children 
  method eval = 
    let dest = self#path
    and tmp = self#tmp_path in 
    if not self#built then (
      f tmp ;
      Sys.rename tmp dest ;
    ) ;
    touch dest ;
    cons dest
  method clean = Sys.remove (* FIXME (path s)*)""
end

let file_cons x = File x

let file path = 
object (self)
  inherit dep ("guizmin.file.input", string "path" path) `file []
  method clean = ()
  method built = Sys.file_exists path
  method eval = 
    assert self#built ;
    File path
end


let f0 id f = path `file  file_cons id f []
let f1 id f x = 
  path 
    `file file_cons id 
    (fun path -> f x#eval path) 
    [ as_dep x ]

let f2 id f x y = 
  path
    `file file_cons id 
    (fun path -> f x#eval y#eval path) 
    [ as_dep x ; as_dep y ]

let dir path =
object (self)
  inherit dep ("guizmin.dir.input", string path) `dir []
  method eval = 
    assert self#built ;
    Dir path
end

let dir_cons x = Dir x

let d0 id f = 
  path `dir dir_cons id f []

let d1 id f x = 
  path 
    `dir dir_cons id 
    (fun path -> f x#eval path) 
    [ as_dep x ]

let d2 id f x y = 
  path 
    `dir dir_cons id 
    (fun path -> f x#eval y#eval path) 
    [ as_dep x ; as_dep y ]

let select x subpath = 
object
  inherit dep ("guizmin.select", string "subpath" subpath) `select [ as_dep x ]
  method eval = 
    let Dir x_path = x#eval in 
    let p = Filename.concat x_path subpath in
    if Sys.file_exists p 
    then File p
    else (
      let msg = sprintf "Tried to access %s in %s but there is no such file or directory." subpath x_path in
      failwith msg
    )
end

let merge l = 
object
  inherit dep "guizmin.merge" `merge (List.map as_dep l)
  method eval = List.map (fun x -> x#eval) l
end

(* type _ pipeline = *)
(*     Input  : path -> (unit, 'a) pipeline *)
(*   | File   : string * param list * (unit -> unit) * 'a pipeline -> ('a, path) pipeline *)
(*   | File2  : string * param list * (unit -> unit) * 'a pipeline * 'b pipeline -> ('a * 'b, path) pipeline *)
(*   | Value  : string * param list * ('a -> 'b) * 'a pipeline -> ('a,'b) pipeline *)
(*   | Value2 : string * param list * (('a * 'b) -> 'c) * 'a pipeline * 'b pipeline -> ('a * 'b,'c) pipeline *)
(*   | Select : path * 'a pipeline -> 'b pipeline *)
(*   | Merge  : 'a pipeline list -> 'a list pipeline *)



