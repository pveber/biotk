open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]

val chromosome_sequences : genome -> [`ucsc_chromosome_sequences] dir
val genome_sequence : genome -> [`fasta] file
val genome_2bit_sequence : genome -> [`ucsc_2bit] file

val fasta_of_bed : genome -> 'a Bed.file -> [`fasta] file




















