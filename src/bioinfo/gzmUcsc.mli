open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]

val chromosome_sequences : genome -> [`ucsc_chromosome_sequences] dir pipeline
val genome_sequence : genome -> [`fasta] file pipeline
val genome_2bit_sequence : genome -> [`ucsc_2bit] file pipeline

val fasta_of_bed : genome -> [`bed] file pipeline -> [`fasta] file pipeline




















