(** GFF format

    This is a module to handle the {i General Feature Format}, a
    widely-used file format to represent transcriptome
    annotations. There are two versions of it currently in use, GFF2
    (also known as GTF) and GFF3 (most often called GFF). They differ
    at the syntax level in the encoding of attributes (see below) and
    GFF3 introduces other, more semantic, differences (see
    {{:http://gmod.org/wiki/GFF2#Why_GFF2_is_harmful_to_your_health}this
    summary}).

    @see <http://gmod.org/wiki/GFF3>
    @see <http://gmod.org/wiki/GFF2>
    @see <https://github.com/The-Sequence-Ontology/Specifications/blob/master/gff3.md>
    @see <https://www.ensembl.org/info/website/upload/gff.html>
    @see <https://agat.readthedocs.io/en/latest/gxf.html#the-gtf-gff-formats>
*)

module Record : sig
  type t = {
    seqname    : string ;
    source     : string option ;
    feature    : string option ;
    start_pos  : int ;
    stop_pos   : int ;
    score      : float option ;
    strand     : [`Plus | `Minus | `Not_stranded | `Unknown ] ;
    phase      : int option ;
    attributes : (string * string list) list ;
  }
  [@@deriving sexp]

  val loc : t -> GLoc.t
  val length : t -> int
  val attribute_exn : t -> string -> string
end

val record :
  ?source:string ->
  ?feature:string ->
  ?score:float -> ?strand:[ `Plus | `Minus | `Not_stranded | `Unknown ] ->
  ?phase:int ->
  ?attributes:(string * string list) list ->
  string ->
  int ->
  int ->
  Record.t

type item = [
  | `Comment of string
  | `Record of Record.t
]

module type S = sig
  include Line_oriented.S with type item := item

  module Item : sig
    type t = item
    val parse : Line.t -> [> `Comment of string | `Record of Record.t ]
    val unparse : item -> string
  end
end

module GFF2 : S
module GFF3 : S

val angstrom_parser :
  [`gff2 | `gff3] ->
  item list Angstrom.t

module Annotation : sig
  open Core

  type t

  val of_items :
    ?gene_id_label:string ->
    ?transcript_id_label:string ->
    item list ->
    t

  val genes : t -> Gene.t String.Table.t * (string * Error.t) list
  val utr3' : t -> Record.t String.Table.t
  val utr5' : t -> Record.t String.Table.t
end
