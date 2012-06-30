type path = string
type param = string * [`int of int | `string of string | `float of float | `bool of bool ]
type id = string * param list

val string : string -> string -> param
val int : string -> int -> param
val float : string -> float -> param
val bool : string -> bool -> param

type 'a pipeline
val eval : 'a pipeline -> 'a

val v0 : id -> (unit -> 'a) -> 'a pipeline
val v1 : id -> ('a -> 'b) -> 'a pipeline -> 'b pipeline
val v2 : id -> ('a -> 'b -> 'c) -> 'a pipeline -> 'b pipeline -> 'c pipeline

type 'a file = private File of path
val file : path -> 'a file pipeline
val f0 : id -> (path -> unit) -> 'a file pipeline
val f1 : id -> ('a -> path -> unit) -> 'a pipeline -> 'b file pipeline
val f2 : id -> ('a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c file pipeline

type 'a dir  = private Dir of path
val dir : path -> 'a dir pipeline
val d0 : id -> (path -> unit) -> 'a dir pipeline
val d1 : id -> ('a -> path -> unit) -> 'a pipeline -> 'b dir pipeline
val d2 : id -> ('a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c dir pipeline

val select : 'a dir pipeline -> path -> 'b file pipeline
val merge : 'a pipeline list -> 'a list pipeline




