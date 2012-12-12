open Guizmin

type path = string list
type item
val item : ?descr:string -> path -> 'a pipeline -> item

val create : ?force:bool -> string -> item list -> unit










