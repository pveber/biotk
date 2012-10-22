open Guizmin

type output

val run : Bowtie.index file -> [`fastq] file list -> output dir
val aligned_reads : output dir -> [`bam] file
val junctions : output dir -> Bed.track file
