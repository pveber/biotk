(** Range Traversal *)

val find :
  int ->
  int ->
  f:(int -> bool) ->
  int option

val find_map :
  int ->
  int ->
  f:(int -> 'a option) ->
  'a option

val count :
  int ->
  int ->
  f:(int -> bool) ->
  int

val iter :
  int ->
  int ->
  f:(int -> unit) ->
  unit

val fold :
  int ->
  int ->
  init:'a ->
  f:('a -> int -> 'a) ->
  'a

val for_all :
  int ->
  int ->
  f:(int -> bool) ->
  bool
