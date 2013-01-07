open Guizmin

type path = string list
type item = private Item : 'a Guizmin.pipeline * string * path -> item

val item : ?descr:string -> path -> 'a pipeline -> item

(** if [np] is greater than 0, builds the members of the repo before
    linking them *)
val create : ?np:int -> base:base_directory -> repo_base:string -> item list -> unit












