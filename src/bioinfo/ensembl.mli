type species = [
| `mus_musculus
]
type genome = [`mm9]

val reference_genome : release:int -> species:species -> genome
val gtf : release:int -> species:species -> Gtf.file


















