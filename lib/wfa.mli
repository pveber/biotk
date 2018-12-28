module type Symbol = sig
  type t
  val all : t list
  val to_string : t -> string
end

module type Score = sig
  type t
  val zero : t
  val compare : t -> t -> int
  val ( + ) : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
end

module type Profile = sig
  type symbol
  type t
  type score
  val score : t -> symbol -> score
end

module type S = sig
  type expression
  type automaton
  type score
  type profile

  val profile : profile -> expression
  val disjunction : expression list -> expression
  val sequence : expression list -> expression
  val gap : min_length:int -> max_length:int -> expression
  (* val tandem : expression -> expression -> int -> int -> expression *)
  (* val map : expression -> f:(profile -> profile) -> expression *)

  val min_score : expression -> score
  val max_score : expression -> score
  val scores : expression -> score array

  (* val string_of_expression : expression -> string *)
  val automaton : expression -> automaton
end

module Make
    (Score : Score)
    (Symbol : Symbol)
    (Profile : Profile with type score = Score.t
                        and type symbol = Symbol.t)
  : S with type score := Score.t
       and type profile := Profile.t

module Nucleotide_profile : sig
  include Profile with type t = float * float * float * float
  val complement : t -> t
end

module Nucleotide_IUPAC : sig
  include Profile
  (* val complement : t -> t *)
  (* val a : t
   * val c : t
   * val g : t
   * val t : t
   * val make : t list -> t *)
end

module PSSM : sig
  include S with type profile := Nucleotide_profile.t
             and type score := float

  val of_counts : ?prior:float -> int array array -> expression
  val tandem : expression -> expression -> int -> int -> expression
end
