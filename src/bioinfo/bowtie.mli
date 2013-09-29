open GzmCore

type index

val index : ?packed:bool -> Fasta.file -> index dir

type 'a quality_encoding = 'a Fastq.format constraint 'a = [< `solexa | `sanger | `phred64 ]

val align_with_maq_policy :
  ?l:int -> ?e:int -> ?m:int ->
  ?qual_kind:'a quality_encoding ->
  n:int -> index dir -> 'a Fastq.file list ->
  Sam.file

val align :
  ?m:int ->
  ?qual_kind:'a quality_encoding ->
  v:int -> index dir -> 'a Fastq.file list ->
  Sam.file




















