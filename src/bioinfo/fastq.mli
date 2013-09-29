open Guizmin

type 'a format = private 'a
val sanger : [`sanger] format
val solexa : [`solexa] format
val phred64 : [`phred64] format

type 'a file = 'a format Guizmin.file

type sanger = [`sanger] file


val nbreads : 'a file -> int pipeline
