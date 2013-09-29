open Guizmin

type output

val run : Bowtie.index dir -> [`sanger] Fastq.file list -> output dir
val aligned_reads : output dir -> Bam.file
val junctions : output dir -> Bed.track file
