type path = string
type param = string * [`int of int | `string of string | `float of float | `bool of bool ]
type id = string * param list
type 'a pipeline

val v0 : id -> (unit -> 'a) -> 'a pipeline
val v1 : id -> ('a -> 'b) -> 'a pipeline -> 'b pipeline
val v2 : id -> ('a -> 'b -> 'c) -> 'a pipeline -> 'b pipeline -> 'c pipeline

type 'a file = private File of string
val file : path -> 'a file pipeline
val f0 : id -> (path -> unit) -> 'a file pipeline
val f1 : id -> ('a -> path -> unit) -> 'a pipeline -> 'b file pipeline
val f2 : id -> ('a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c file pipeline

type 'a dir  = private Dir of string
val dir : path -> 'a dir pipeline
val d0 : id -> (path -> unit) -> 'a dir pipeline
val d1 : id -> ('a -> path -> unit) -> 'a pipeline -> 'b dir pipeline
val d2 : id -> ('a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c dir pipeline

val select : 'a dir pipeline -> path -> 'b file pipeline
val merge : 'a pipeline list -> 'a list pipeline

type descr = [
  `input of path 
| `step of id * descr list
| `select of descr * path
| `merge of descr list
]
val descr : 'a pipeline -> descr
val digest : 'a pipeline -> string

(* type _ pipeline = *)
(*     Input  : path -> (unit, 'a) pipeline *)
(*   | File   : string * param list * (unit -> unit) * 'a pipeline -> ('a, path) pipeline *)
(*   | File2  : string * param list * (unit -> unit) * 'a pipeline * 'b pipeline -> ('a * 'b, path) pipeline *)
(*   | Value  : string * param list * ('a -> 'b) * 'a pipeline -> ('a,'b) pipeline *)
(*   | Value2 : string * param list * (('a * 'b) -> 'c) * 'a pipeline * 'b pipeline -> ('a * 'b,'c) pipeline *)
(*   | Select : path * 'a pipeline -> 'b pipeline *)
(*   | Merge  : 'a pipeline list -> 'a list pipeline *)



