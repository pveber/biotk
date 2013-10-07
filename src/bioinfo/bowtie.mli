open GzmCore

type index

val index : ?packed:bool -> Fasta.file -> index dir

val align_with_maq_policy :
  ?l:int -> ?e:int -> ?m:int ->
  ?fastq_format:'a Fastq.format ->
  n:int -> index dir -> 'a Fastq.file list ->
  Sam.file

val align :
  ?m:int ->
  ?fastq_format:'a Fastq.format ->
  v:int -> index dir -> 'a Fastq.file list ->
  Sam.file
