type path = string

module Param : sig
  type t
  val string : string -> string -> t
  val int : string -> int -> t
  val float : string -> float -> t
  val bool : string -> bool -> t
  val opt : (string -> 'a -> t) -> string -> 'a option -> t
end

type id = string
type params = Param.t list

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

type 'a pipeline

val hash : 'a pipeline -> string

val v0 : id -> params -> (env -> 'a) -> 'a pipeline
val v1 : id -> params -> 'a pipeline -> (env -> 'a -> 'b) -> 'b pipeline
val v2 : id -> params -> 'a pipeline -> 'b pipeline -> (env -> 'a -> 'b -> 'c) -> 'c pipeline
val v3 : id -> params -> 'a pipeline -> 'b pipeline -> 'c pipeline -> (env -> 'a -> 'b -> 'c -> 'd) -> 'd pipeline

type 'a file_path = private File of path
type 'a file = 'a file_path pipeline
val file : path -> 'a file
val f0 : id -> params -> (env -> path -> unit) -> 'a file
val f1 : id -> params -> 'a pipeline -> (env -> 'a -> path -> unit) -> 'b file
val f2 : id -> params -> 'a pipeline -> 'b pipeline -> (env -> 'a -> 'b -> path -> unit) -> 'c file
val f3 : id -> params -> 'a pipeline -> 'b pipeline -> 'c pipeline -> (env -> 'a -> 'b -> 'c -> path -> unit) -> 'd file

type 'a dir_path  = private Dir of path
type 'a dir = 'a dir_path pipeline
val dir : path -> 'a dir
val d0 : id -> params -> (env -> path -> unit) -> 'a dir
val d1 : id -> params -> 'a pipeline -> (env -> 'a -> path -> unit) -> 'b dir
val d2 : id -> params -> 'a pipeline -> 'b pipeline -> (env -> 'a -> 'b -> path -> unit) -> 'c dir
val d3 : id -> params -> 'a pipeline -> 'b pipeline -> 'c pipeline -> (env -> 'a -> 'b -> 'c -> path -> unit) -> 'd dir

val select : 'a dir -> path -> 'b file
val merge : 'a pipeline list -> 'a list pipeline
val adapter : 'a pipeline -> ('a -> 'b) -> 'b pipeline

type base_directory = private string
val base_directory : string -> base_directory
val default_base_directory : unit -> base_directory

exception Error of string * exn

val build : 
  ?base:base_directory -> ?np:int ->
  'a pipeline -> unit
val eval : 
  ?base:base_directory -> ?np:int ->
  'a pipeline -> 'a
val path : base:base_directory -> 'a pipeline -> string

















