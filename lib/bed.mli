(** BED format

    BED (Browser Extensible Data) format is used to describe a
    collection of genomic features. It is a tabulated format. See the
    {{https://genome.ucsc.edu/FAQ/FAQformat.html#format1}specification}. Several
    variants exist, depending on the number of fields, and they are
    accordingly named BED3, BED4, {i etc}. This module offers one
    representation per variant, including a general-purpose
    representation only assuming 3 fields.
*)

type strand = [
  | `Plus
  | `Minus
  | `Not_relevant
  | `Unknown
]
val parse_strand : string -> (strand, string) result
val unparse_strand : strand -> string

module type Item = sig
  type t
  val loc : t -> GLoc.t
  val of_line : Line.t -> t
  val to_line : t -> string
end

module type S = sig
  type item
  val load : string -> item list
  val load_as_lmap : string -> item GAnnot.LMap.t
  val save : item list -> string -> unit
end

module Item : Item with type t = GLoc.t * string list

include S with type item := Item.t

module Bed3 : sig
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  module Item : sig
    include Item with type t = item
    val of_loc : GLoc.t -> t
  end
  include S with type item := Item.t
end


module Bed4 : sig
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  module Item : Item with type t = item
  include S with type item := Item.t
end

module Bed5 : sig
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
  }
  module Item : sig
    include Item with type t = item
    val to_bed4 : item -> Bed4.item
  end
  include S with type item := Item.t
end

module Bed6 : sig
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
    strand : strand ;
  }
  module Item : Item with type t = item
  include S with type item := Item.t
end
