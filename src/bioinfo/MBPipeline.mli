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

  module Transcriptome : sig
    val gtf : (genome, Gtf.file) assoc
  end

  module TF_ChIP_seq : sig
    val factors : string list

    (** conditions for which there is at least one ChIP *)
    val conditions : condition list
    val input_controls : sample list
    val chIP_samples : sample list
    val samples : sample list

    val chIP_samples_by_factor_and_condition : (string * condition, sample list) assoc
    val input_controls_by_condition : (condition, sample list) assoc

    val fastq_files : (sample, [`fastq] Guizmin.file list) assoc
    val aligned_reads : (sample, Sam.file) assoc
    val macs_peaks_wo_control : pvalue:float -> (sample, Macs.Wo_control.Peak.file) assoc
    val macs_peaks_with_control : pvalue:float -> (sample * sample, Macs.With_control.Peak.file) assoc
  end

  module RNA_seq : sig
    val conditions : condition list
    val samples : sample list
    val samples_by_condition : (condition, sample list) assoc

    val fastq_files : (sample, [`fastq] Guizmin.file list) assoc
    val aligned_reads : (sample, Bam.file) assoc
    val counts : (sample, Htseq.Output.file) assoc
  end

  val repo : Guizmin_repo.item list

  module Config_file_check : sig
    type error = [
    | `duplicate_condition_id of string
    | `duplicate_sample_id of string
    ]
    val errors : error list
    val error_msg : string list
  end
end



















