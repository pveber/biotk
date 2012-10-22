open Guizmin

type output

val run14 : 
  genomesize:int value -> tagsize:int -> bandwidth:int -> 
  pvalue:float -> 
  ?input:[`bam] file -> [`bam] file -> output dir
