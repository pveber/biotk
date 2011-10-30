type path = string
type param = string * [`int of int | `string of string | `float of float | `bool of bool ]
type id = string * param list

type descr = [
  `input of path 
| `step of id * descr list
| `select of descr * path
| `merge of descr list
]

type 'a pipeline = < clean : unit ; 
                     descr : descr ;
		     eval  : 'a >

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

let digest x = Digest.to_hex (Digest.string (Marshal.to_string x#descr []))

let path x = 
  cache_dir ^ "/" ^ (digest x)

let tmp_path x = 
  tmp_dir ^ "/" ^ (digest x)

let load_value path = 
  let ic = open_in path in 
  let r = input_value ic in
  close_in r ; r

let save_value v path = 
  let oc = open_out path in 
  output_value oc v ;
  close_out oc 

let value id f descr_children = 
object (s)
  method descr = `step (id, descr_children)
  method eval = 
    let dest = path s 
    and tmp = tmp_path s in 
    if not (Sys.file_exists dest) then (
      let r = f () in
      save_value r tmp ;
      Sys.rename tmp dest ;
      r
    )
    else load_value dest
  method clean = Sys.remove (path s)
end

let l1 id f (x :'a pipeline) : 'b pipeline = 
  value id (fun () -> f x#eval) [ x#descr ]

let l2 id f x y = 
  value id (fun () -> f x#eval y#eval) [ x#descr ; y#descr ]

type 'a file = File of string
type 'a dir  = Dir of string

let fs0 cons path = object
  method descr = `input path
  method eval = 
    assert (Sys.file_exists path) ;
    cons path
  method clean = ()
end

let fs id f descr_children cons =
object (s)
  method descr = `step (id, descr_children)
  method eval = 
    let dest = path s 
    and tmp = tmp_path s in 
    if not (Sys.file_exists dest) then (
      f tmp ;
      Sys.rename tmp dest ;
    ) ;
    cons dest
  method clean = Sys.remove (path s)
end

let file x = File x
let f0 = fs0 file
let f1 id f x = 
  fs id (f x#eval) [ x#descr ] file
let f2 id f x y = 
  fs id (f x#eval y#eval) [ x#descr ; y#descr ] file

let dir x = Dir x
let d0 = fs0 dir
let d1 id f x = 
  fs id (f x#eval) [ x#descr ] dir
let d2 id f x y = 
  fs id (f x#eval y#eval) [ x#descr ; y#descr ] dir

let select x subpath = 
object
  method descr = `select (x#descr, subpath)
  method eval = 
    let p = (path x) ^ "/" ^ subpath in
    assert (Sys.file_exists p) ;
    File p
  method clean = ()
end

let merge l = 
object
  method descr = `merge (List.map (fun x -> x#descr) l)
  method eval = List.map (fun x -> x#eval) l
  method clean = ()
end


let descr x = x#descr

(* type _ pipeline = *)
(*     Input  : path -> (unit, 'a) pipeline *)
(*   | File   : string * param list * (unit -> unit) * 'a pipeline -> ('a, path) pipeline *)
(*   | File2  : string * param list * (unit -> unit) * 'a pipeline * 'b pipeline -> ('a * 'b, path) pipeline *)
(*   | Value  : string * param list * ('a -> 'b) * 'a pipeline -> ('a,'b) pipeline *)
(*   | Value2 : string * param list * (('a * 'b) -> 'c) * 'a pipeline * 'b pipeline -> ('a * 'b,'c) pipeline *)
(*   | Select : path * 'a pipeline -> 'b pipeline *)
(*   | Merge  : 'a pipeline list -> 'a list pipeline *)



