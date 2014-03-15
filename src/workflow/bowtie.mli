open Types

type index = [`bowtie_index] directory

val package : package workflow

val bowtie_build :
  ?packed:bool ->
  ?color:bool  ->
  fasta workflow -> index workflow

val bowtie :
  ?l:int -> ?e:int -> ?m:int ->
  ?fastq_format:'a Fastq.format ->
  ?n:int -> ?v:int ->
  index workflow -> 'a Fastq.workflow list -> sam workflow
