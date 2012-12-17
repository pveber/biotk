open Guizmin
open MBSchema

type output

val run14 : 
  genomesize:int pipeline -> tagsize:int -> bandwidth:int -> 
  pvalue:float -> 
  ?control:Bam.file -> Bam.file -> output dir

type peak = private {
  loc : Location.t ;
  length : int ;
  summit : int ;
  tags : int ;
  pvalue : float ;
  fold : float ;
  fdr : float option ;
}

val peaks : output dir -> peak Guizmin_table.file
val peak_parser : peak Guizmin_table.file_path -> peak BatEnum.t

val bed : output dir -> Bed.minimal_file















