open Guizmin

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

val sanger : [`sanger] format
val solexa : [`solexa] format
val phred64 : [`phred64] format

type 'a file = 'a format Guizmin.file

type sanger = [`sanger] file

val sanger_of_solexa : [`solexa] file -> [`sanger] file
val nbreads : 'a file -> int pipeline
