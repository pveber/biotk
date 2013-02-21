open Guizmin

type species = [
| `mus_musculus
]
type genome = [`mm9]

val reference_genome : release:int -> species:species -> genome
type gtf = private Gtf.file
val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> gtf

module Annotation : sig
  open MBSchema
  val transcripts : gtf -> Transcript.t list pipeline
  val genes : gtf -> Gene.t list pipeline
end



















