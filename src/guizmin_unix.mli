open Guizmin

val wget : string -> 'a file
val gunzip : [`gzipped of 'a] file -> 'a file
val unzip : [`zipped of 'a ] file -> 'a dir
val crlf2lf : 'a file -> 'a file



















