type cmd = string * string list
type 'a logger = ('a,unit,string,unit) format4 -> 'a

val string_of_cmd : cmd -> string

val sp : ('a, unit, string) format -> 'a

val sh :
  ?debug:(string -> unit) logger ->
  ?error:(string -> unit) logger ->
  ?stdout:out_channel ->
  ?stderr:out_channel -> ('a, unit, string, unit) format4 -> 'a
(** Shell invocation. Raises [Failure] in case of a non zero return
    code. *)

val shout :
  ?debug:(string -> unit) logger ->
  ?error:(string -> unit) logger ->
  ?stderr:out_channel -> ('a, unit, string, string) format4 -> 'a
(** Same as [sh] except that [shout] returns the standard output as string *)

val bash : ?debug:bool -> ?stdout:out_channel -> ?stderr:out_channel -> string list -> unit

(** [pipefail cmd1 cmd2] does a special pipe of commands [cmd1] and [cmd2]
    whose exit code is non null as soon as that of any of the two commands 
    is. 
*)
val pipefail : string -> string -> string

val save : string -> 'a -> unit
val load : string -> 'a

val lines_of_file : string -> string list
val lines_to_file : string -> string Stream.t -> unit














