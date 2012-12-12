open Guizmin

type path = string list
type item
val item : descr:string -> path -> 'a pipeline -> item

val create : ?force:bool -> item list -> string -> unit
