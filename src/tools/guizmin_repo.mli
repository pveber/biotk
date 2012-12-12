open Guizmin

type path = string list
type item
val item : descr:string -> path -> 'a pipeline -> item

type t
val make : item list -> t
val build : t -> string -> unit
