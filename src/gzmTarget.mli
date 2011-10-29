type t = 
    Input of string
  | Rule of string option * script * deps
  | PartOf of t * string
and script = string list
and deps = t list

type 'a file = private t
type 'a dir = private t

val input : string -> 'a file
val rule : ?path:string list -> script -> deps -> 'a file
val partof : 'a dir -> string -> 'b file


type term = Target of target_id * term list
and target_id = string * param list
and param = string * [ `S of string | `I of int | `F of float ]

type dep = < path : string ; term : term ; clean : unit >

module F : sig
  type +'a t = private < abstract : dep ; 
			 path : string ; 
			 term : term ;
			 clean : unit ; .. >

  val make  : target_id -> dep list -> (string -> unit) -> 'a t

  val input : string -> 'a t
end

module V : sig
  type +'a t = private < abstract : dep ; 
			 value : 'a ; 
			 clean : unit ; 
			 path  : string ; 
			 term  : term ; .. >

  val make : target_id -> dep list -> (unit -> 'a) -> 'a t
end
