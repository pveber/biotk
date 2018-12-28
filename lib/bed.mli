open Biocaml_base

type 'a item = [
  | `Comment of string
  | `Record of 'a
]

type strand = [
  | `Plus
  | `Minus
  | `Not_relevant
  | `Unknown
]
val parse_strand : string -> (strand, string) result
val unparse_strand : strand -> string

module type S = sig
  type t
  val loc : t -> GLoc.t
  val of_line : Line.t -> t item
  val to_line : t item -> string
  val load : string -> t item list
  val load_as_lmap : string -> t GAnnot.LMap.t
end

module Bed3 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  include S with type t := t
end


module Bed4 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  include S with type t := t
end

module Bed5 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
  }
  include S with type t := t
  val to_bed4 : t item -> Bed4.t item
end

module Bed6 : sig
  type t = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : float ;
    strand : strand ;
  }
  include S with type t := t
end
