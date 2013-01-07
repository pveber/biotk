open Guizmin

type path = string list
type item = private Item : 'a Guizmin.pipeline * string * path -> item

val item : ?descr:string -> path -> 'a pipeline -> item

val create : base:string -> repo_base:string -> item list -> unit




















