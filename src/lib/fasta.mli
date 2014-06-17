open Core_kernel.Std

type item = {
  comment : string ;
  seq : string
}

val of_channel : in_channel  -> item Or_error.t Stream.t
(* val to_channel : out_channel -> item Stream.t -> unit *)
