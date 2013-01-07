open Printf

type path = string

module Param = struct
  type t =
    | Int of string * int
    | String of string * string
    | Float of string * float
    | Bool of string * bool
    | Option of string * t option

  let string id v = String (id, v)
  let int id v = Int (id, v)
  let float id v = Float (id, v)
  let bool id b = Bool (id, b)
  let opt f id = function
  | Some v -> Option (id, Some (f id v))
  | None -> Option (id, None)
end

type id = string
type params = Param.t list

type 'a file_path = File of path
type 'a dir_path  = Dir of path

type env = {
  base : string ;
  sh : 'a. ('a,unit,string,unit) format4 -> 'a ;
  bash : string list -> unit ;
  stdout : out_channel ;
  stderr : out_channel ;
  debug : 'a. ('a,unit,string,unit) format4 -> 'a ;
  info  : 'a. ('a,unit,string,unit) format4 -> 'a ;
  error : 'a. ('a,unit,string,unit) format4 -> 'a ;
  np : int ;
  mem : int ; (** in MB *)
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
  hash = make_hash id params `value [ x.hash ] ;
  kind = Val1 (x, f)
}

let v2 id params x y f = {
  id ; params ;
  hash = make_hash id params `value [ x.hash ; y.hash ] ;
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
  hash = make_hash id params `file [ x.hash ] ;
  kind = File1 (x, f)
}

let f2 id params x y f = {
  id ; params ;
  hash = make_hash id params `file [ x.hash ; y.hash ] ;
  kind = File2 (x, y, f)
}

let f3 id params x y z f = {
  id ; params ;
  hash = make_hash id params `file [ x.hash ; y.hash ; z.hash ] ;
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
  hash = make_hash id params `dir [ x.hash ] ;
  kind = Dir1 (x, f)
}

let d2 id params x y f = {
  id ; params ;
  hash = make_hash id params `dir [ x.hash ; y.hash ] ;
  kind = Dir2 (x, y, f)
}

let d3 id params x y z f = {
  id ; params ;
  hash = make_hash id params `dir [ x.hash ; y.hash ; z.hash ] ;
  kind = Dir3 (x, y, z, f)
}

let select dir subpath = 
  let id = "guizmin.select" in
  let params = [Param.string "subpath" subpath] in
  {
    id ; params ;
    hash = make_hash id params `select [ dir.hash ] ;
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


(* let home_dir    = Sys.getcwd () *)

let cache_dir base = base ^ "/cache"
let build_dir base = base ^ "/_build"
let tmp_dir base = base ^ "/tmp"
let stderr_dir base = base ^ "/stderr"
let stdout_dir base = base ^ "/stdout"
let log_dir base = base ^ "/logs"

let path ~base x = 
  cache_dir base ^ "/" ^ (x.hash)

let tmp_path ~base x = 
  tmp_dir base ^ "/" ^ (x.hash)

let touch fn =
  ignore (Sys.command ("touch " ^ fn))

let with_null_env base ~f =
  let stderr = open_out "/dev/null" in
  let stdout = open_out "/dev/null" in
  let log (type s) (fmt : (s, unit, string, unit) format4) = ksprintf ignore fmt in
  let env = { 
      base ; stderr ; stdout ; np = 1 ; mem = 100 ; 
      debug = log ;
      info = log ;
      error = log ;
      sh = log ;
      bash = ignore
  }
  in
  let r = f env in
  List.iter close_out [ stderr ; stdout ] ;
  r

let with_env ?(np = 1) ?(mem = 100) base x ~f =
  let stderr = open_out (sprintf "%s/%s" (stderr_dir base) x.hash) in
  let stdout = open_out (sprintf "%s/%s" (stdout_dir base) x.hash) in
  let log_chan = open_out (sprintf "%s/%s" (log_dir base) x.hash) in
  let log (type s) label (fmt : (s, unit, string, unit) format4) =
    let open Unix in
    let f msg =
      let t = localtime (time ()) in
      fprintf 
        log_chan "[%s][%4d/%2d/%2d %2d:%2d] %s" 
        label t.tm_year t.tm_mon t.tm_mday t.tm_hour t.tm_min msg
    in
    ksprintf f fmt in
    let env = { 
      base ; stderr ; stdout ; np ; mem ; 
      debug = (fun fmt -> log "DEBUG" fmt) ; 
      info = (fun fmt -> log "INFO" fmt) ; 
      error = (fun fmt -> log "ERROR" fmt) ;
      sh = GzmUtils.sh ;
      bash = GzmUtils.bash ~stdout ~stderr 
    }
    in
    let r = f env in
    List.iter close_out [ log_chan ; stderr ; stdout ] ;
    r

type 's update = { f : 'x. 's -> 'x pipeline -> 's }

let rec fold : type a. 's update -> 's -> a pipeline -> 's = fun f init x -> 
  match x.kind with 
  | Val0 _ -> f.f init x 
  | Val1 (y,_) -> f.f (fold f init y) x
  | Val2 (y,z,_) -> f.f (fold f (fold f init y) z) x
  | Val3 (y,z,w,_) -> f.f (fold f (fold f (fold f init y) z) w) x
  | File_input _ -> f.f init x
  | File0 _ -> f.f init x
  | File1 (y,_) ->  f.f (fold f init y) x
  | File2 (y,z,_) -> f.f (fold f (fold f init y) z) x
  | File3 (y,z,w,_) -> f.f (fold f (fold f (fold f init y) z) w) x
  | Dir_input _ -> f.f init x
  | Dir0 _ -> f.f init x
  | Dir1 (y,_) -> f.f (fold f init y) x
  | Dir2 (y,z,_) -> f.f (fold f (fold f init y) z) x
  | Dir3 (y,z,w,_) -> f.f (fold f (fold f (fold f init y) z) w) x
  | Merge xs -> f.f (List.fold_left f.f init xs) x
  | Select (_,dir) -> f.f (fold f init dir) x


let built : type a. base:string -> a pipeline -> bool = fun ~base x ->
  Sys.file_exists (path ~base x)

let rec unsafe_eval : type a. base:string -> a pipeline -> a = fun ~base x ->
  match x.kind with
  | Val0 _ | Val1 _ | Val2 _ | Val3 _ -> load_value (path base x)
  | File_input path -> File path
  | File0 _ -> File (path base x) 
  | File1 _ -> File (path base x) 
  | File2 _ -> File (path base x) 
  | File3 _ -> File (path base x) 
  | Dir_input path -> Dir path
  | Dir0 _ -> Dir (path base x) 
  | Dir1 _ -> Dir (path base x) 
  | Dir2 _ -> Dir (path base x) 
  | Dir3 _ -> Dir (path base x) 
  | Merge xs -> List.map (unsafe_eval ~base) xs
  | Select (subpath, dir) ->
      let Dir dir_path = unsafe_eval ~base dir in 
      let p = Filename.concat dir_path subpath in
      File p
        
        
let exec : type a. a pipeline -> env -> unit = fun x env ->
  let val_wrap x f =
    let dest = path ~base:env.base x in
    let tmp = tmp_path ~base:env.base x in
    if not (file_exists dest) then (
      let r = f env in
      save_value r tmp ;
      Sys.rename tmp dest
    )
    else touch dest
  in
  let path_wrap x f =
    let dest = path ~base:env.base x in
    let tmp = tmp_path ~base:env.base x in 
    if not (Sys.file_exists dest) then (
      f env tmp ;
      Sys.rename tmp dest ;
    ) ;
    touch dest
  in
  let eval : type a. a pipeline -> a = fun x -> unsafe_eval ~base:env.base x in
  match x.kind with
  | Val0 f -> val_wrap x f
  | Val1 (x1, f) -> val_wrap x (fun env -> f env (eval x1))
  | Val2 (x1, x2, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2))
  | Val3 (x1, x2, x3, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))
      
  | File_input path ->
      if not (file_exists path) then (
        fprintf env.stderr "File %s is declared as an input of a pipeline but does not exist." path ;
        assert false
      )
      
  | File0 f -> path_wrap x f
  | File1 (x1, f) -> path_wrap x (fun env -> f env (eval x1))
  | File2 (x1, x2, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2))
  | File3 (x1, x2, x3, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | Dir_input path ->
      if not (dir_exists path) then (
        fprintf env.stderr "Directory %s is declared as an input of a pipeline but does not exist." path ;
        assert false
      )

  | Dir0 f -> path_wrap x f
  | Dir1 (x1, f) -> path_wrap x (fun env -> f env (eval x1))
  | Dir2 (x1, x2, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2))
  | Dir3 (x1, x2, x3, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | Select (subpath, dir) ->
      let Dir dir_path = eval dir in 
      let p = Filename.concat dir_path subpath in
      if not (Sys.file_exists p) then (
        let msg = sprintf "Tried to access %s in %s but there is no such file or directory." subpath dir_path in
        failwith msg
      )

  | Merge xs -> ()

type base_directory = string

let create_base_directory base =
  mkdir base ;
  mkdir (cache_dir base) ;
  mkdir (build_dir base) ;
  mkdir (stderr_dir base) ;
  mkdir (stdout_dir base) ;
  mkdir (log_dir base) ;
  mkdir (tmp_dir base)

let base_directory base =
  create_base_directory base ;
  base

let default_base_directory () =
  base_directory (Sys.getcwd () ^ "/_guizmin")

let build : type a. ?base:string -> ?np:int -> a pipeline -> unit = fun ?(base = Sys.getcwd ()) ?(np = 1) x ->
  with_null_env base ~f:(fun null ->
    let update = { f = (
      fun () x -> 
        if not (built ~base:null.base x)
        then with_env ~np base x ~f:(exec x)
    ) } 
    in 
    fold update () x
  )

let eval : type a. ?base:string -> ?np:int -> a pipeline -> a = fun ?(base = Sys.getcwd ()) ?(np = 1) x ->
  build ~base ~np x ;
  with_null_env base ~f:(fun null -> unsafe_eval ~base:null.base x)





















