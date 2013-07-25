open Printf
open GzmUtils
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
  with_temp_file : 'a. (string -> 'a) -> 'a ;
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
  | Val4 : 'b pipeline * 'c pipeline * 'd pipeline * 'e pipeline * (env -> 'b -> 'c -> 'd -> 'e -> 'a) -> 'a kind
  | Val5 : 'b pipeline * 'c pipeline * 'd pipeline * 'e pipeline * 'f pipeline * (env -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'a kind
  | Val6 : 'b pipeline * 'c pipeline * 'd pipeline * 'e pipeline * 'f pipeline * 'g pipeline * (env -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) -> 'a kind

  | File_input : path -> 'a file_path kind
  | File0 : (env -> path -> unit) -> 'a file_path kind
  | File1 : 'b pipeline * (env -> 'b -> path -> unit) -> 'a file_path kind
  | File2 : 'b pipeline * 'c pipeline * (env -> 'b -> 'c -> path -> unit) -> 'a file_path kind
  | File3 : 'b pipeline * 'c pipeline * 'd pipeline * (env -> 'b -> 'c -> 'd -> path -> unit) -> 'a file_path kind
  | File5 : 'b pipeline * 'c pipeline * 'd pipeline * 'e pipeline * 'f pipeline * (env -> 'b -> 'c -> 'd -> 'e -> 'f -> path -> unit) -> 'a file_path kind

  | Dir_input : path -> 'a dir_path kind
  | Dir0 : (env -> path -> unit) -> 'a dir_path kind
  | Dir1 : 'b pipeline * (env -> 'b -> path -> unit) -> 'a dir_path kind
  | Dir2 : 'b pipeline * 'c pipeline * (env -> 'b -> 'c -> path -> unit) -> 'a dir_path kind
  | Dir3 : 'b pipeline * 'c pipeline * 'd pipeline * (env -> 'b -> 'c -> 'd -> path -> unit) -> 'a dir_path kind

  | Select : path * 'a dir_path pipeline -> 'b file_path kind

  | Merge : 'a pipeline list -> 'a list kind

  | Adapter : 'a pipeline * ('a -> 'b) -> 'b kind

  | Map : 'a list pipeline * ('a pipeline -> 'b pipeline) -> 'b list kind

type 'a file = 'a file_path pipeline
type 'a dir = 'a dir_path pipeline
type pipeline_path = { path : 'a. 'a pipeline -> string }

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

let hash x = x.hash

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

let v3 id params x y z f = {
  id ; params ;
  hash = make_hash id params `value [ x.hash ; y.hash ; z.hash ] ;
  kind = Val3 (x, y, z, f)
}

let v4 id params x y z w f = {
  id ; params ;
  hash = make_hash id params `value [ x.hash ; y.hash ; z.hash ; w.hash ] ;
  kind = Val4 (x, y, z, w, f)
}

let v5 id params x y z w r f = {
  id ; params ;
  hash = make_hash id params `value [ x.hash ; y.hash ; z.hash ; w.hash ; r.hash ] ;
  kind = Val5 (x, y, z, w, r, f)
}

let v6 id params x y z w r s f = {
  id ; params ;
  hash = make_hash id params `value [ x.hash ; y.hash ; z.hash ; w.hash ; r.hash ; s.hash ] ;
  kind = Val6 (x, y, z, w, r, s, f)
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

let f5 id params x y z w r f = {
  id ; params ;
  hash = make_hash id params `file [ x.hash ; y.hash ; z.hash ; w.hash ; r.hash ] ;
  kind = File5 (x, y, z, w, r, f)
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
    hash = make_hash id [] `merge (List.map hash files) ;
    kind = Merge files
  }

let adapter x f =
  let id = "guizmin.adapter" in
  {
    id ; params = [] ;
    hash = make_hash id [] `merge [ x.hash ] ;
    kind = Adapter (x,f)
  }

let map x f =
  let id = "guizmin.map" in
  let fake = v0 "guizmin.internal_fake" [] (fun _ -> assert false) in
  let fake_image = f fake in
  {
    id ; params = [] ;
    hash = make_hash id [] `map [ x.hash ; fake_image.hash ] ;
    kind = Map (x, f)
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
let history_dir base = base ^ "/history"

let rec string_descr : type s. ?tab:int -> s pipeline -> string = fun ?(tab = 0) x ->
  let rec string_of_param = Param.(
    function
    | Int (k,v) -> sprintf "%s=%d" k v
    | String (k,v) -> sprintf "%s=%s" k v
    | Float (k,v) -> sprintf "%s=%f" k v
    | Option (id,None) -> id
    | Option (_,Some t) -> string_of_param t
    | Bool (k,v) -> sprintf "%s=%b" k v
  )
  in
  let space = String.make tab ' ' in
  sprintf "%s%s(%s)%s"
    space
    x.id
    (String.concat "," (List.map string_of_param x.params))
    (
      match x.kind with
      | Val0 _ -> "[]"
      | Val1 (x1,_) ->
          sprintf "[\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            space
      | Val2 (x1,x2,_) ->
          sprintf "[\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            space
      | Val3 (x1,x2,x3,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            space
      | Val4 (x1,x2,x3,x4,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            (string_descr ~tab:(tab + 2) x4)
            space
      | Val5 (x1,x2,x3,x4,x5,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            (string_descr ~tab:(tab + 2) x4)
            (string_descr ~tab:(tab + 2) x5)
            space
      | Val6 (x1,x2,x3,x4,x5,x6,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            (string_descr ~tab:(tab + 2) x4)
            (string_descr ~tab:(tab + 2) x5)
            (string_descr ~tab:(tab + 2) x6)
            space

      | File_input _ -> "[]"
      | File0 _ -> "[]"
      | File1 (x1,_) ->
          sprintf "[\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            space
      | File2 (x1,x2,_) ->
          sprintf "[\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            space
      | File3 (x1,x2,x3,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            space
      | File5 (x1,x2,x3,x4,x5,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            (string_descr ~tab:(tab + 2) x4)
            (string_descr ~tab:(tab + 2) x5)
            space

      | Dir_input _ -> "[]"
      | Dir0 _ -> "[]"
      | Dir1 (x1,_) ->
          sprintf "[\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            space
      | Dir2 (x1,x2,_) ->
          sprintf "[\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            space
      | Dir3 (x1,x2,x3,_) -> 
          sprintf "[\n%s\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) x2)
            (string_descr ~tab:(tab + 2) x3)
            space

      | Select (_,dir) ->
          sprintf "[\n%s\n%s]"
            (string_descr ~tab:(tab + 2) dir)
            space
            
      | Merge xs ->
          sprintf "[\n%s\n%s]"
            (String.concat ",\n" (List.map (string_descr ~tab:(tab + 2)) xs))
            space

      | Adapter (x1,_) ->
          sprintf "[\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            space
          
      | Map (x1, f) ->
          let fake = v0 "guizmin.internal_fake" [] (fun _ -> assert false) in
          let fake_image = f fake in
          sprintf "[\n%s\n%s\n%s]"
            (string_descr ~tab:(tab + 2) x1)
            (string_descr ~tab:(tab + 2) fake_image)
            space
    )

let path : type s. base:string -> s pipeline -> string = fun ~base x ->
  let cache_path = cache_dir base ^ "/" ^ x.hash in
  match x.kind with
  | Merge _ -> invalid_arg "Guizmin.path: a merge pipeline is not physically saved"
  | Adapter _ -> invalid_arg "Guizmin.path: an adapter pipeline is not physically saved"
  | Select (subpath, dir) ->
      cache_dir base ^ "/" ^ dir.hash ^ "/" ^ subpath
  | File_input path -> path
  | Dir_input path -> path
  | Val0 _ | Val1 _ | Val2 _ | Val3 _ | Val4 _ | Val5 _ | Val6 _ -> cache_path
  | File0 _ -> cache_path
  | File1 _ -> cache_path
  | File2 _ -> cache_path
  | File3 _ -> cache_path
  | File5 _ -> cache_path
  | Dir0 _ -> cache_path
  | Dir1 _ -> cache_path
  | Dir2 _ -> cache_path
  | Dir3 _ -> cache_path
  | Map (x,_) -> cache_path

let tmp_path ~base x =
  tmp_dir base ^ "/" ^ (x.hash)

let touch fn =
  ignore (Sys.command ("touch " ^ fn))

let bash ~(debug: 'a logger) ~(error: 'a logger) ~stdout ~stderr cmds =
  let script = String.concat "\n" cmds in
  debug "bash call:\n\n%s\n\n" script ;
  try
    Shell.call
      ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout))
      ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
      [ Shell.cmd "bash" [ "-c" ; String.concat " && " cmds ] ]
  with Shell.Subprocess_error _ -> (
    error "bash call exited with non-zero code:\n\n%s\n\n" script ;
    Core.Std.failwithf "bash shell call failed:\n%s\n" script ()
  )

let with_env ?(np = 1) ?(mem = 100) base x ~f =
  let stderr = open_out (sprintf "%s/%s" (stderr_dir base) x.hash) in
  let stdout = open_out (sprintf "%s/%s" (stdout_dir base) x.hash) in
  let log_chan = open_out (sprintf "%s/%s" (log_dir base) x.hash) in
  let log (type s) label (fmt : (s, unit, string, unit) format4) =
    let open Unix in
    let f msg =
      let t = localtime (time ()) in
      fprintf
        log_chan "[%s][%04d-%02d-%02d %02d:%02d] %s%!"
        label (1900 + t.tm_year) (t.tm_mon + 1)t.tm_mday t.tm_hour t.tm_min msg
    in
    ksprintf f fmt in
  let debug fmt = log "DEBUG" fmt in
  let info fmt = log "INFO" fmt in
  let error fmt = log "ERROR" fmt in
  let env = {
    base ; stderr ; stdout ; np ; mem ;
    debug ; info ; error ;
    sh = (fun fmt -> sh ~debug ~error ~stdout ~stderr fmt) ;
    bash = bash ~debug ~error ~stdout ~stderr ;
    with_temp_file = fun f -> GzmUtils.with_temp_file ~in_dir:(tmp_dir base) ~f
  }
  in
  let r = f env in
  List.iter close_out [ log_chan ; stderr ; stdout ] ;
  r

type 's update = { 
  guard : 'x. 'x pipeline -> bool ;
  f : 'x. 's -> 'x pipeline -> 's ;
}

let rec fold : type a. 's update -> 's -> a pipeline -> 's = fun up init x ->
  if up.guard x then
    match x.kind with
    | Val0 _ -> up.f init x
    | Val1 (y,_) -> up.f (fold up init y) x
    | Val2 (y,z,_) -> up.f (fold up (fold up init y) z) x
    | Val3 (y,z,w,_) -> up.f (fold up (fold up (fold up init y) z) w) x
    | Val4 (x1,x2,x3,x4,_) -> up.f (fold up (fold up (fold up (fold up init x1) x2) x3) x4) x
    | Val5 (x1,x2,x3,x4,x5,_) -> up.f (fold up (fold up (fold up (fold up (fold up init x1) x2) x3) x4) x5) x
    | Val6 (x1,x2,x3,x4,x5,x6,_) -> up.f (fold up (fold up (fold up (fold up (fold up (fold up init x1) x2) x3) x4) x5) x6) x
    | File_input _ -> up.f init x
    | File0 _ -> up.f init x
    | File1 (y,_) ->  up.f (fold up init y) x
    | File2 (y,z,_) -> up.f (fold up (fold up init y) z) x
    | File3 (y,z,w,_) -> up.f (fold up (fold up (fold up init y) z) w) x
    | File5 (y,z,w,r,s,_) -> up.f (fold up (fold up (fold up (fold up (fold up init y) z) w) r) s) x
    | Dir_input _ -> up.f init x
    | Dir0 _ -> up.f init x
    | Dir1 (y,_) -> up.f (fold up init y) x
    | Dir2 (y,z,_) -> up.f (fold up (fold up init y) z) x
    | Dir3 (y,z,w,_) -> up.f (fold up (fold up (fold up init y) z) w) x
    | Merge xs -> up.f (List.fold_left (fold up) init xs) x
    | Select (_,dir) -> up.f (fold up init dir) x
    | Adapter (y,_) -> up.f (fold up init y) x
    | Map (y,_) -> up.f (fold up init y) x
  else
    init

let fold_deps : type a. 's update -> 's -> a pipeline -> 's = fun up init x ->
  if up.guard x then 
    match x.kind with
    | Val0 _ -> init
    | Val1 (y,_) -> up.f init y
    | Val2 (y,z,_) -> up.f (up.f init y) z
    | Val3 (y,z,w,_) -> up.f (up.f (up.f init y) z) w
    | Val4 (x1,x2,x3,x4,_) -> up.f (up.f (up.f (up.f init x1) x2) x3) x4
    | Val5 (x1,x2,x3,x4,x5,_) -> up.f (up.f (up.f (up.f (up.f init x1) x2) x3) x4) x5
    | Val6 (x1,x2,x3,x4,x5,x6,_) -> up.f (up.f (up.f (up.f (up.f (up.f init x1) x2) x3) x4) x5) x6
    | File_input _ -> init
    | File0 _ -> init
    | File1 (y,_) ->  up.f init y
    | File2 (y,z,_) -> up.f (up.f init y) z
    | File3 (y,z,w,_) -> up.f (up.f (up.f init y) z) w
    | File5 (y,z,w,r,s,_) -> up.f (up.f (up.f (up.f (up.f init y) z) w) r) s
    | Dir_input _ -> init
    | Dir0 _ -> init
    | Dir1 (y,_) -> up.f init y
    | Dir2 (y,z,_) -> up.f (up.f init y) z
    | Dir3 (y,z,w,_) -> up.f (up.f (up.f init y) z) w
    | Merge xs -> List.fold_left up.f init xs
    | Select (_,dir) -> up.f init dir
    | Adapter (y,_) -> up.f init y
    | Map (y,_) -> up.f init y
  else
    init

let rec built : type a. base:string -> a pipeline -> bool = fun ~base x ->
  match x.kind with
  | Merge xs -> List.for_all (built ~base) xs
  | Select (_,dir) -> built ~base dir
  | Adapter (y,_) -> built ~base y
  | Map _ -> Sys.file_exists (path ~base x)
  | Val0 _ | Val1 _ | Val2 _ | Val3 _ | Val4 _ | Val5 _ | Val6 _ ->
      Sys.file_exists (path ~base x)
  | File_input _ -> Sys.file_exists (path ~base x)
  | File0 _ -> Sys.file_exists (path ~base x)
  | File1 _ -> Sys.file_exists (path ~base x)
  | File2 _ -> Sys.file_exists (path ~base x)
  | File3 _ -> Sys.file_exists (path ~base x)
  | File5 _ -> Sys.file_exists (path ~base x)
  | Dir_input _ -> Sys.file_exists (path ~base x)
  | Dir0 _ -> Sys.file_exists (path ~base x)
  | Dir1 _ -> Sys.file_exists (path ~base x)
  | Dir2 _ -> Sys.file_exists (path ~base x)
  | Dir3 _ -> Sys.file_exists (path ~base x)

let append_history ~base ~msg x =
  let date_stamp = CalendarLib.(Printer.Date.to_string (Date.today ())) in
  sh "echo %s: %s s>> %s/%s" date_stamp msg (history_dir base) x.hash

let rec history : type a. base:string -> msg:string -> a pipeline -> unit = fun ~base ~msg x ->
  let log x = append_history ~base ~msg x in
  match x.kind with
  | Merge xs -> List.iter (history ~base ~msg) xs
  | Select (_,dir) -> history ~base ~msg dir
  | Adapter (y,_) -> history ~base ~msg y
  | Map _ -> log x
  | Val0 _ | Val1 _ | Val2 _ | Val3 _ | Val4 _ | Val5 _ | Val6 _ ->
      log x
  | File_input _ -> log x
  | File0 _ -> log x
  | File1 _ -> log x
  | File2 _ -> log x
  | File3 _ -> log x
  | File5 _ -> log x
  | Dir_input _ -> log x
  | Dir0 _ -> log x
  | Dir1 _ -> log x
  | Dir2 _ -> log x
  | Dir3 _ -> log x

let log_used  ~base = history ~base ~msg:"USED"
let log_built ~base = history ~base ~msg:"BUILT"
let log_requested ~base = history ~base ~msg:"REQ"

let rec unsafe_eval : type a. base:string -> a pipeline -> a = fun ~base x ->
  match x.kind with
  | Val0 _ | Val1 _ | Val2 _ | Val3 _ | Val4 _ | Val5 _ | Val6 _ -> load_value (path base x)
  | Map _ -> load_value (path base x)
  | File_input path -> File path
  | File0 _ -> File (path base x)
  | File1 _ -> File (path base x)
  | File2 _ -> File (path base x)
  | File3 _ -> File (path base x)
  | File5 _ -> File (path base x)
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
  | Adapter (x,f) -> f (unsafe_eval ~base x)

(* pipeline execution. only accepts pipelines for which there is  actual work to do. 
   Special pipelines raise Invalid_argument *)
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
  | Val4 (x1, x2, x3, x4, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3) (eval x4))
  | Val5 (x1, x2, x3, x4, x5, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3) (eval x4) (eval x5))
  | Val6 (x1, x2, x3, x4, x5, x6, f) -> val_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3) (eval x4) (eval x5) (eval x6))

  | File_input path ->
      if not (file_exists path) then (
        let msg = sp "File %s is declared as an input of a pipeline but does not exist." path in
        env.error "%s" msg ;
        failwith msg
      )

  | File0 f -> path_wrap x f
  | File1 (x1, f) -> path_wrap x (fun env -> f env (eval x1))
  | File2 (x1, x2, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2))
  | File3 (x1, x2, x3, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))
  | File5 (x1, x2, x3, x4, x5, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3) (eval x4) (eval x5))

  | Dir_input path ->
      if not (dir_exists path) then (
        let msg = sp "Directory %s is declared as an input of a pipeline but does not exist." path in
        env.error "%s" msg ;
        failwith msg
      )

  | Dir0 f -> path_wrap x f
  | Dir1 (x1, f) -> path_wrap x (fun env -> f env (eval x1))
  | Dir2 (x1, x2, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2))
  | Dir3 (x1, x2, x3, f) -> path_wrap x (fun env -> f env (eval x1) (eval x2) (eval x3))

  | Select (subpath, dir) -> raise (Invalid_argument "GzmCore.exec: select")
  | Merge xs -> raise (Invalid_argument "GzmCore.exec: merge")
  | Adapter _ -> raise (Invalid_argument "GzmCore.exec: adapter")
  | Map _ -> raise (Invalid_argument "GzmCore.exec: map")


type base_directory = string

let create_base_directory base =
  mkdir base ;
  mkdir (cache_dir base) ;
  mkdir (build_dir base) ;
  mkdir (stderr_dir base) ;
  mkdir (stdout_dir base) ;
  mkdir (log_dir base) ;
  mkdir (tmp_dir base) ;
  mkdir (history_dir base)


let base_directory base =
  create_base_directory base ;
  base

let default_base_directory () =
  base_directory (Sys.getcwd () ^ "/_guizmin")

let list_nth l n =
  v1
    "guizmin.list_nth"
    [ Param.int "n" n ]
    l
    (fun _ l -> List.nth l n)

exception Error of string * exn

let rec build : type a. ?base:string -> ?np:int -> a pipeline -> unit = fun ?(base = Sys.getcwd ()) ?(np = 1) x ->
  let update = { 
    guard = (
      fun (type s) (x : s pipeline) ->
        (* if the pipeline is built, mark it as used *)
        if not (built ~base x) 
        then true
        else (log_used  ~base x ; false)
    ) ;
    f = (
      fun (type s) () (x : s pipeline) ->
        match x.kind with
        | Map (y, f) ->
            let n = List.length (unsafe_eval ~base y) in
            let args = Core.Std.List.init n ~f:(list_nth y) in
            let r = merge (List.map f args) in
            build ~base ~np r ;
            save_value (unsafe_eval ~base r) (path ~base x)
        | Select (subpath, dir) ->
            let Dir dir_path = unsafe_eval ~base dir in
            let p = Filename.concat dir_path subpath in
            if not (Sys.file_exists p) then (
              let msg = sprintf "Tried to access %s in %s but there is no such file or directory." subpath dir_path in
              failwith msg
            )
        | Merge _ -> ()
        | Adapter _ -> ()
        | _ ->
            with_env ~np base x ~f:(fun env ->
              try
                env.info "\n%s\n" (string_descr x) ;
                exec x env ;
                log_built ~base x
              with e -> (
                env.error "failed to eval pipeline with hash %s and id %s" x.hash x.id ;
                raise e
              )
            )
    ) ;
  }
  in
  fold update () x

let eval : type a. ?base:string -> ?np:int -> a pipeline -> a = fun ?(base = Sys.getcwd ()) ?(np = 1) x ->
  log_requested ~base x ;
  build ~base ~np x ;
  unsafe_eval ~base x


















