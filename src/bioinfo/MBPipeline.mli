open Guizmin

type ('a, 'b) assoc = ('a * 'b) list

module Make(P : sig 
                  val config_file : MBSchema.ConfigFile.t 
                end) : 
sig
  open MBSchema.ConfigFile

  val conditions : condition list
  val samples : sample list
  val genomes : genome list

  module Genome : sig
    val bowtie_index : (genome, Bowtie.index dir) assoc
  end

  module TF_ChIP_seq : sig
    val samples : sample list
    val aligned_reads : (sample, Bam.file) assoc
  end
end



















