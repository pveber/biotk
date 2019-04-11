open Core_kernel
open Biocaml_base

module Item : sig
  type t = Gff.item
  include Line_oriented.Item with type t := t
  val to_record : t -> Gff.record option
end

module Record : sig
  type t = Gff.record
  val loc : t -> GLoc.t
end

include Line_oriented.S with type item = Item.t

module Annotation : sig
  type t

  val of_items :
    id_label:string ->
    Item.t list ->
    t

  val genes : t -> (string * Gene.t) list Or_error.t
end
