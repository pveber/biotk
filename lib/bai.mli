open Unsigned
open Stdio

type chunk = {
  chunk_beg : UInt64.t ;
  chunk_end : UInt64.t ;
}

type bin = {
  bin : int ;
  n_chunk : int ;
  chunks : chunk array ;
}

type interval = Ioffset of UInt64.t [@@unboxed]

type reference_sequence = {
  n_bin : int ;
  bins : bin array ;
  n_intv : int ;
  intervals : interval array ;
}

type t = {
  n_ref : int ;
  reference_sequences : reference_sequence array ;
  n_no_coor : UInt64.t option ;
}

val read : In_channel.t -> (t, [> `Msg of string]) result
