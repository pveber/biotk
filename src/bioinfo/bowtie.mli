open GzmCore

type index

val index : ?packed:bool -> Fasta.file -> index file

val align_with_maq_policy :
  ?l:int -> ?e:int -> ?m:int ->
  ?qual_kind:[`phred33|`phred64|`solexa] ->
  n:int -> index file -> [ `fastq] file list ->
  Bam.file

val align :
  ?m:int ->
  ?qual_kind:[`phred33|`phred64|`solexa] ->
  v:int -> index file -> [ `fastq] file list ->
  Bam.file




















