type species = [
| `mus_musculus
]
type genome = [`mm9]

val reference_genome : release:int -> species:species -> genome
val gtf : ?chr_name : [`ensembl | `ucsc] -> release:int -> species:species -> Gtf.file




















