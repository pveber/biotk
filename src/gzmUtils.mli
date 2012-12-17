val sp : ('a, unit, string) format -> 'a

val sh : ('a,unit,string,unit) format4 -> 'a
(** Shell invocation. Raises [Failure] in case of a non zero return
    code. *)

val bash : ?debug:bool -> ?stdout:out_channel -> ?stderr:out_channel -> string list -> unit

val save : string -> 'a -> unit
val load : string -> 'a

val lines_of_file : string -> string list
val lines_to_file : string -> string Stream.t -> unit
