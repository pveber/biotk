open Base

type t = private {
  chr : string ;
  strand : [ `Plus | `Minus ] ;
  exons : (int * int) list ;
}

val make :
  strand:[ `Plus | `Minus ] ->
  exons:GLoc.t list ->
  t Or_error.t

val introns : t -> GLoc.t list
