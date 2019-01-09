open Biocaml_unix

val fold0 :
  bam:string ->
  bai:string ->
  loc:GLoc.t ->
  init:'a ->
  f:('a -> Bam.Header.t -> Bam.Alignment0.t -> 'a) ->
  ('a, [> `Msg of string]) result
