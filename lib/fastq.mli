open Core_kernel
open Biocaml_unix

type item = Fastq.item

val fold_file :
  string ->
  init:'a ->
  f:('a -> item -> 'a) ->
  'a Or_error.t
