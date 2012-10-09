val sp : ('a, unit, string) format -> 'a

val sh : ('a,unit,string,unit) format4 -> 'a
(** Shell invocation. Raises [Failure] in case of a non zero return
    code. *)

val bash : ?debug:bool -> ?stdout:out_channel -> ?stderr:out_channel -> string list -> unit




















