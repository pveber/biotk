open GzmCore

val index : ?packed:bool -> [`fasta] file pipeline -> [`bowtie_index] file pipeline

val align : 
  ?n:int -> ?l:int -> ?e:int -> ?m:int -> ?p:int -> 
  ?qual_kind:[`phred33|`phred64|`solexa] ->
  [`bowtie_index] file pipeline -> [ `fastq] file list pipeline -> 
  [`sam ] file pipeline

(* val align_nomaq :  *)
(*   ?v:int -> ?m:int -> ?p:int ->  *)
(*   [`bowtie_index] file pipeline -> [ `fastq] file list pipeline ->  *)
(*   [`sam ] file format *)

