open Biocaml_base

module Item : sig
  type t = Gff.item
  include Line_oriented.Item with type t := t
  val to_record : t -> Gff.record option
end

include Line_oriented.S with type item = Item.t
