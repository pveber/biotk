open Core.Std

type ('a, 'b) assoc = ('a * 'b) list
let assoc l ~f = List.map ~f:(fun x -> x, f x) l
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
    let samples = extract (
      function
      | Sample ({ sample_type = TF_ChIP_seq _ } as sample) -> 
          Some sample
      | _ -> None
    )

    let fastq_files : (sample, [`fastq] Guizmin.file list) assoc = 
      assoc samples (
        fun s -> List.map s.sample_files ~f:Guizmin.file
      )

    let aligned_reads = assoc samples (
      fun s -> 
        let genome = (model s.sample_model).model_genome in
        Bowtie.align ~v:2 ~m:1 (Genome.bowtie_index & genome) (fastq_files & s)
    )
  end

  let samples =
    List.concat [
      TF_ChIP_seq.samples
    ]
end



















