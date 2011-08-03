type term = Target of target_id * term list
and target_id = string * param list
and param = string * [ `S of string | `I of int | `F of float ]

type dep = < path : string ; term : term ; clean : unit >

module F = struct
  type +'a t = < abstract : dep ; 
		 path : string ; 
		 term : term ;
		 clean : unit >

  let make tid dl f = assert false

  let input path = assert false
end

module V = struct 
  type +'a t = < abstract : dep ; 
		 value : 'a ; 
		 clean : unit ; 
		 path  : string ; 
		 term  : term >

  let make tid dl f = assert false
end
