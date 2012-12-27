open Printf

type path = string

module Param = struct
  type t =
    | Int : string * int -> t
    | String : string * string -> t
    | Float : string * float -> t
    | Bool : string * bool -> t
    | Option : (string -> 'a -> t) * string * 'a option -> t

  let string id v = String (id, v)
  let int id v = Int (id, v)
  let float id v = Float (id, v)
  let bool id b = Bool (id, b)
  let opt f id v = Option (f, id, v)
end

type id = string
type params = Param.t list

type 'a file_path = File of path
type 'a dir_path  = Dir of path

type env = {
  bash : string list -> unit ;
  stdout : out_channel ;
  stderr : out_channel ;
  np : int ; 
}

type 'a pipeline = {
  id : string ;
  params : Param.t list ;
  hash : string ;
  kind : 'a kind ;
}
and _ kind =
  | Val0 : (env -> 'a) -> 'a kind
  | Val1 : 'b pipeline * (env -> 'b -> 'a) -> 'a kind
  | Val2 : 'b pipeline * 'c pipeline * (env -> 'b -> 'c -> 'a) -> 'a kind
  | Val3 : 'b pipeline * 'c pipeline * 'd pipeline * (env -> 'b -> 'c -> 'd -> 'a) -> 'a kind

  | File_input : path -> 'a file_path kind
  | File0 : (env -> path -> unit) -> 'a file_path kind
  | File1 : 'b pipeline * (env -> 'b -> path -> unit) -> 'a file_path kind
  | File2 : 'b pipeline * 'c pipeline * (env -> 'b -> 'c -> path -> unit) -> 'a file_path kind
  | File3 : 'b pipeline * 'c pipeline * 'd pipeline * (env -> 'b -> 'c -> 'd -> path -> unit) -> 'a file_path kind

  | Dir_input : path -> 'a dir_path kind
  | Dir0 : (env -> path -> unit) -> 'a dir_path kind
  | Dir1 : 'b pipeline * (env -> 'b -> path -> unit) -> 'a dir_path kind
  | Dir2 : 'b pipeline * 'c pipeline * (env -> 'b -> 'c -> path -> unit) -> 'a dir_path kind
  | Dir3 : 'b pipeline * 'c pipeline * 'd pipeline * (env -> 'b -> 'c -> 'd -> path -> unit) -> 'a dir_path kind

  | Select : path * 'a dir_path pipeline -> 'b file_path kind

  | Merge : 'a pipeline list -> 'a list kind

(* type _ pipeline = *)
(*     Input  : path -> (unit, 'a) pipeline *)
(*   | File   : string * param list * (unit -> unit) * 'a pipeline -> ('a, path) pipeline *)
(*   | File2  : string * param list * (unit -> unit) * 'a pipeline * 'b pipeline -> ('a * 'b, path) pipeline *)
(*   | Value  : string * param list * ('a -> 'b) * 'a pipeline -> ('a,'b) pipeline *)
(*   | Value2 : string * param list * (('a * 'b) -> 'c) * 'a pipeline * 'b pipeline -> ('a * 'b,'c) pipeline *)
(*   | Select : path * 'a pipeline -> 'b pipeline *)
(*   | Merge  : 'a pipeline list -> 'a list pipeline *)

type 'a file = 'a file_path pipeline
type 'a dir = 'a dir_path pipeline

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

let hash x = x.hash

let path x = 
  cache_dir ^ "/" ^ (hash x)

let tmp_path x = 
  tmp_dir ^ "/" ^ (hash x)

let touch fn =
  ignore (Sys.command ("touch " ^ fn))


let digest x =
  Digest.to_hex (Digest.string (Marshal.to_string x []))

let make_hash id params kind deps_hash =
  let content = id, params, kind, deps_hash in
  digest content

let v0 id params f = {
  id ; params ;
  hash = make_hash id params `value [] ;
  kind = Val0 f
}

let v1 id params x f = {
  id ; params ;
  hash = make_hash id params `value [ hash x ] ;
  kind = Val1 (x, f)
}

let v2 id params x y f = {
  id ; params ;
  hash = make_hash id params `value [ hash x ; hash y ] ;
  kind = Val2 (x, y, f)
}

let file path = 
  let id = "guizmin.file.input" in
  let params = [ Param.string "path" path ] in 
  {
    id ; params ;
    hash = make_hash id params `file_input [] ;
    kind = File_input path
  }

let f0 id params f = {
  id ; params ;
  hash = make_hash id params `file [] ;
  kind = File0 f
}

let f1 id params x f = {
  id ; params ;
  hash = make_hash id params `file [ hash x ] ;
  kind = File1 (x, f)
}

let f2 id params x y f = {
  id ; params ;
  hash = make_hash id params `file [ hash x ; hash y ] ;
  kind = File2 (x, y, f)
}

let f3 id params x y z f = {
  id ; params ;
  hash = make_hash id params `file [ hash x ; hash y ; hash z ] ;
  kind = File3 (x, y, z, f)
}


let dir path = 
  let id = "guizmin.dir.input" in
  let params = [ Param.string "path" path ] in 
  {
    id ; params ;
    hash = make_hash id params `file_input [] ;
    kind = Dir_input path
  }

let d0 id params f = {
  id ; params ;
  hash = make_hash id params `dir [] ;
  kind = Dir0 f
}

let d1 id params x f = {
  id ; params ;
  hash = make_hash id params `dir [ hash x ] ;
  kind = Dir1 (x, f)
}

let d2 id params x y f = {
  id ; params ;
  hash = make_hash id params `dir [ hash x ; hash y ] ;
  kind = Dir2 (x, y, f)
}

let d3 id params x y z f = {
  id ; params ;
  hash = make_hash id params `dir [ hash x ; hash y ; hash z ] ;
  kind = Dir3 (x, y, z, f)
}

let select dir subpath = 
  let id = "guizmin.select" in
  let params = [Param.string "subpath" subpath] in
  {
    id ; params ;
    hash = make_hash id params `select [ hash dir ] ;
    kind = Select (subpath, dir)
  }

let merge files =
  let id = "guizmin.merge" in
  {
    id ; params = [] ;
    hash = make_hash id [] `merge [] ;
    kind = Merge files
  }

let load_value path = 
  let ic = open_in path in 
  let r = input_value ic in
  close_in ic ; r

let save_value v path = 
  let oc = open_out path in 
  output_value oc v ;
  close_out oc 

(* FIXME: we should also check that [path] is indeed a regular file *)
let file_exists path = Sys.file_exists path

(* FIXME: we should also check that [path] is indeed a directory *)
let dir_exists path = Sys.file_exists path

let rec eval : type a. a pipeline -> a = function x ->
  let env = { stderr ; stdout ; np = 1 ; 
              bash = fun l -> GzmUtils.bash ~stdout ~stderr l } in
  let val_wrap x f () =
    let dest = path x in
    let tmp = tmp_path x in
    if not (file_exists dest) then (
      let r = f env in
      save_value r tmp ;
      Sys.rename tmp dest ;
      r
    )
    else (
      touch dest ;
      load_value dest
    )
  in

  let path_wrap cons x f () =
    let dest = path x in
    let tmp = tmp_path x in 
    if not (Sys.file_exists dest) then (
      f env tmp ;
      Sys.rename tmp dest ;
    ) ;
    touch dest ;
    cons dest
  in
  let file_wrap = path_wrap (fun x -> File x) in
  let dir_wrap = path_wrap (fun x -> Dir x) in
  
  let f : unit -> a = match x.kind with
  | Val0 f -> val_wrap x f
  | Val1 (x1, f) -> val_wrap x (fun env -> f env (eval x1))
  | Val2 (x1, x2, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2))
  | Val3 (x1, x2, x3, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | File_input path ->
      fun () -> 
        if not (file_exists path) then (
          fprintf env.stderr "File %s is declared as an input of a pipeline but does not exist." path ;
          assert false
        ) ;
        File path
      
  | File0 f -> file_wrap x f
  | File1 (x1, f) -> file_wrap x (fun env -> f env (eval x1))
  | File2 (x1, x2, f) -> file_wrap x (fun env -> f env (eval x1) (eval x2))
  | File3 (x1, x2, x3, f) -> file_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | Dir_input path ->
      fun () -> 
        if not (dir_exists path) then (
          fprintf env.stderr "Directory %s is declared as an input of a pipeline but does not exist." path ;
          assert false
        ) ;
        Dir path

  | Dir0 f -> dir_wrap x f
  | Dir1 (x1, f) -> dir_wrap x (fun env -> f env (eval x1))
  | Dir2 (x1, x2, f) -> dir_wrap x (fun env -> f env (eval x1) (eval x2))
  | Dir3 (x1, x2, x3, f) -> dir_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | Select (subpath, dir) ->
      fun () ->
        let Dir dir_path = eval dir in 
        let p = Filename.concat dir_path subpath in
        if Sys.file_exists p 
        then File p
        else (
          let msg = sprintf "Tried to access %s in %s but there is no such file or directory." subpath dir_path in
          failwith msg
        )

  | Merge xs ->
      fun () -> List.map eval xs

  in
  f ()




















