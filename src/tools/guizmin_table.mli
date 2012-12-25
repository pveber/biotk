open Guizmin

include module type of Guizmin_table_sig

val with_rows :
  (module Row with type t = 'a) ->
  ?header:bool ->
  ?sep:char ->
  'a file_path -> f:('a Stream.t -> 'b) -> 'b
  
val load : 
  (module Row with type t = 'a) ->
  (module Table with type t = 'b) ->
  ?header:bool -> ?sep:char -> 'a file_path -> 'b

module Make : F
