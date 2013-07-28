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
    include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty
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
    include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty
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

val best_peaks : n:int -> 'a Wo_control.Peak.file' -> 'a Wo_control.Peak.file'
(** [best_peaks ~n peaks] builds a peak file keeping only the [n]
    peaks with maximum - log10 pval in [peaks]. If [peaks] has less
    than [n] peaks, take them all. *)



















