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
