open Guizmin

type path = string list
type item = private Item : 'a Guizmin.pipeline * string * path -> item

val item : ?descr:string -> path -> 'a pipeline -> item

(** if [np] is greater than 0, builds the members of the repo before
    linking them. If [repo_base] already exists, use it otherwise
    mkdir it. If wipeout then remove any previous contents of
    [repo_base]. *)
val create : ?np:int -> ?wipeout:bool -> base:base_directory -> repo_base:string -> item list -> unit












