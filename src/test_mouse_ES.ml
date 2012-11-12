#use "topfind"
#thread
#require "guizmin.bioinfo"

open Guizmin_bioinfo

let sample x = Sra.(fastq_dump (sample x))

let sox2_fastq = List.map sample [ "SRR002023" (* ; "SRR002024" ; "SRR002025" "SRR002025" *) ]
let gfp_fastq = List.map sample [ "SRR001996" ; "SRR001997" ; "SRR001998" ; "SRR001999" ]

let bowtie_index_mm9 = Bowtie.index (Ucsc.genome_sequence `mm9)

let sox2_bam = Bowtie.align ~v:2 bowtie_index_mm9 sox2_fastq
let gfp_bam = Bowtie.align ~v:2 bowtie_index_mm9 gfp_fastq

let sox2_peaks = Macs.(
  run14 ~genomesize:(Guizmin.v0 ("mm9.length",[]) (fun _ -> 1800000000)) ~tagsize:25
    ~bandwidth:300 ~pvalue:1e-5
    ~control:gfp_bam
    sox2_bam
)

let () = Guizmin.(
  let File p = eval ~np:7 (Macs.peaks sox2_peaks) in
  print_endline p
)


















