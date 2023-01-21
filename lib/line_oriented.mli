(** Line-oriented file formats

    This module features a few primitives that may help dealing with
    file formats that are sequences of items that fit a single line,
    aka line-oriented formats *)

val fold :
  string ->
  init:'a ->
  f:('a -> int -> Line.t -> 'a) ->
  'a

val fold_err :
  string ->
  init:'a ->
  f:('a -> int -> Line.t -> ('a, 'b) result) ->
  ('a, 'b) result

(** A parser for lines

    This module provides building blocks for a line parser, especially
    a {!step} function, that given the state of the parser and a chunk
    of bytes returns a list of lines and a new state. This API is
    meant to be usable from various contexts, including async or lwt.
*)
module Parser : sig
  (** Parser state *)
  type state

  (** The initial state to be fed to the {!step} function. *)
  val initial_state : state

  (** Number of lines seen so far. We have [line_number initial_state
      = 0], and [line_number (fst (step _ (Some _))) > 0] *)
  val line_number : state -> int

  (** [step st i] parses an input. If [i = None], the caller indicates
      that there is no more input; in that case the returned state is
      terminal: all inputs read from this state will be ignored. *)
  val step : state -> string option -> state * Line.t list
end

module type Item = sig
  type t
  val parse : Line.t -> t
  val unparse : t -> string
end

module type S = sig
  type item
  val load : string -> item list
  val fold : string -> init:'a -> f:('a -> item -> 'a) -> 'a
  val save : item list -> string -> unit
end

module Make(Item : Item) : S with type item = Item.t
