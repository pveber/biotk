type path = string

module Param : sig
  type t
  val string : string -> string -> t
  val int : string -> int -> t
  val float : string -> float -> t
  val bool : string -> bool -> t
  val opt : (string -> 'a -> t) -> string -> 'a option -> t
end

type id = string * Param.t list

type env = private {
  bash : string list -> unit ;
  stdout : out_channel ;
  stderr : out_channel ;
  np : int ; 
}

type 'a pipeline
val eval : 
  ?stdout:out_channel -> ?stderr:out_channel -> ?np:int ->
  'a pipeline -> 'a
val path : 'a pipeline -> string

val v0 : id -> (env -> 'a) -> 'a pipeline
val v1 : id -> (env -> 'a -> 'b) -> 'a pipeline -> 'b pipeline
val v2 : id -> (env -> 'a -> 'b -> 'c) -> 'a pipeline -> 'b pipeline -> 'c pipeline

type 'a file_path = private File of path
type 'a file = 'a file_path pipeline
val file : path -> 'a file
val f0 : id -> (env -> path -> unit) -> 'a file
val f1 : id -> (env -> 'a -> path -> unit) -> 'a pipeline -> 'b file
val f2 : id -> (env -> 'a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c file
val f3 : id -> (env -> 'a -> 'b -> 'c -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c pipeline -> 'd file

type 'a dir_path  = private Dir of path
type 'a dir = 'a dir_path pipeline
val dir : path -> 'a dir
val d0 : id -> (env -> path -> unit) -> 'a dir
val d1 : id -> (env -> 'a -> path -> unit) -> 'a pipeline -> 'b dir
val d2 : id -> (env -> 'a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c dir
val d3 : id -> (env -> 'a -> 'b -> 'c -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c pipeline -> 'd dir

val select : 'a dir -> path -> 'b file
val merge : 'a pipeline list -> 'a list pipeline

















