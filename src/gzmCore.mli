type path = string
type param = string * [`int of int | `string of string | `float of float | `bool of bool ]
type id = string * param list

val string : string -> string -> param
val int : string -> int -> param
val float : string -> float -> param
val bool : string -> bool -> param
val opt : (string -> 'a -> param) -> string -> 'a option -> param option
val ( +? ) : 'a list -> 'a option -> 'a list
val ( ++ ) : 'a list -> 'a -> 'a list

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

type 'a dir_path  = private Dir of path
type 'a dir = 'a dir_path pipeline
val dir : path -> 'a dir
val d0 : id -> (env -> path -> unit) -> 'a dir
val d1 : id -> (env -> 'a -> path -> unit) -> 'a pipeline -> 'b dir
val d2 : id -> (env -> 'a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c dir
val d3 : id -> (env -> 'a -> 'b -> 'c -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c pipeline -> 'd dir

val select : 'a dir -> path -> 'b file
val merge : 'a pipeline list -> 'a list pipeline

















