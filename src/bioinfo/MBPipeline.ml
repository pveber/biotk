open Core.Std

type ('a, 'b) assoc = ('a * 'b) list
let assoc l ~f = List.map ~f:(fun x -> x, f x) l
let rel_of_pairs l = Stream.of_list l |! Biocaml_accu.relation |! Biocaml_stream.to_list
let ( & ) l x = List.Assoc.find_exn l x

type genome = MBSchema.ConfigFile.genome

module Make(P : sig 
                  val config_file : MBSchema.ConfigFile.t 
                end) =
struct
  open P
  open MBSchema.ConfigFile

  let extract f = List.filter_map config_file ~f
  let extract' f = BatList.unique (extract f)

  let conditions = extract' (
    function
    | Condition c -> Some c
    | _ -> None
  )

  let models =
    extract' (
      function
      | Model m -> Some m
      | _ -> None
    )

  let genomes = extract' (
    function
    | Model m -> Some m.model_genome
    | _ -> None
  )

  let model id = List.find_exn models ~f:(fun m -> m.model_id = id)

  module Genome = struct
    let sequence = assoc genomes ~f:Ucsc.genome_sequence

    let bowtie_index = assoc genomes ~f:(
      fun g -> Bowtie.index ~packed:true (sequence & g)
    )
  end

  module TF_ChIP_seq = struct
    let factors = extract' (
      function
      | Sample ({ sample_type = TF_ChIP_seq tf }) -> Some tf
      | _ -> None
    )

    let conditions = extract' (
      function
      | Sample ({ sample_condition = c }) -> Some c
      | _ -> None
    )

    let chIP_samples = extract (
      function
      | Sample ({ sample_type = TF_ChIP_seq _ } as sample) -> 
          Some sample
      | _ -> None
    )

    let input_controls = extract (
      function
      | Sample ({ sample_type = ChIP_seq_input } as sample) -> 
          Some sample
      | _ -> None
    )

    let samples = chIP_samples @ input_controls

    let chIP_samples_by_factor_and_condition =
      extract (
        function
        | Sample ({ sample_type = TF_ChIP_seq tf ; sample_condition } as sample) -> 
            Some ((tf, sample_condition), sample)
        | _ -> None
      )
      |! rel_of_pairs

    let input_controls_by_condition =
      extract (
        function
        | Sample ({ sample_type = ChIP_seq_input ; sample_condition } as sample) -> 
            Some (sample_condition, sample)
        | _ -> None
      )
      |! rel_of_pairs

    let fastq_files : (sample, [`fastq] Guizmin.file list) assoc = 
      assoc samples (
        fun s -> List.map s.sample_files ~f:Guizmin.file
      )

    let aligned_reads = assoc samples (
      fun s -> 
        let genome = (model s.sample_model).model_genome in
        Bowtie.align ~v:2 ~m:1 Genome.(bowtie_index & genome) (fastq_files & s)
    )

    let bam_aligned_reads = assoc samples (
      fun s -> Samtools.bam_of_sam (aligned_reads & s)
    )

    let macs_peaks_wo_control ~pvalue = assoc chIP_samples (fun s ->
      let genome = (model s.sample_model).model_genome in
      Macs.Wo_control.(
        run ~genome:genome ~pvalue (bam_aligned_reads & s)
        |! peaks
      )
    )
  end

  let samples =
    List.concat [
      TF_ChIP_seq.samples
    ]

  let repo = List.concat Guizmin_repo.([
    List.map 
      (TF_ChIP_seq.macs_peaks_wo_control ~pvalue:1e-3)
      ~f:(fun (sample, peaks) ->
        let Guizmin.File f = Guizmin.eval ~base:(Guizmin.default_base_directory ()) peaks in
        print_endline f ;
        print_endline sample.sample_id ;
        print_endline (Guizmin.hash peaks) ;
            item 
              ["chIP-seq" ; "peaks" ; "macs" ; "wo_control" ; sample.sample_id ^ ".tsv" ] 
              peaks) ;
    
  ])
end














