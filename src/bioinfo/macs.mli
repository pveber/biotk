open Guizmin

type 'a output

module Wo_control : sig
  module Peak : sig
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
    }
    include module type of Guizmin_table.Make(Row)(Table)
  end
  val run :
    ?tagsize:int -> ?bandwidth:int -> 
    genome:[< Ucsc.genome] -> pvalue:float -> 
    Bam.file -> Peak.t output dir

  val peaks : Peak.t output dir -> Peak.file
end

module With_control : sig
  module Peak : sig
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
      fdr : float ;
    }
    include module type of Guizmin_table.Make(Row)(Table)
  end

  val run : 
    ?tagsize:int -> ?bandwidth:int -> 
    genome:[< Ucsc.genome] -> 
    pvalue:float -> 
    control:Bam.file ->
    Bam.file -> Peak.t output dir

  val peaks : Peak.t output dir -> Peak.file
end

val bed : 'a output dir -> Bed.Basic.file
