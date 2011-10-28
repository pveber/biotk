open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]

val golden_path : genome -> [`goldenPath] dir
val genome_sequence : genome -> [`fasta] file

