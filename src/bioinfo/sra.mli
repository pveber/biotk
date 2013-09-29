open Guizmin

type format

val sample : string -> format file

val fastq_dump : format file -> Fastq.sanger
