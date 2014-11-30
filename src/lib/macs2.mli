open Biocaml

module Narrow_peaks : sig
  type item = {
    loc : Seq_range.t ;
  }

  val read : in_channel -> item Stream.t
end
