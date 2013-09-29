open Guizmin

type report

val report : 'a Fastq.file -> report dir
val html_report : report dir -> [`html] file
