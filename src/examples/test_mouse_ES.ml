(**
   Study of the dataset presented in 
   {v
@article{chen2008integration,
  title={Integration of external signaling pathways with the core transcriptional network in embryonic stem cells},
  author={Chen, X. and Xu, H. and Yuan, P. and Fang, F. and Huss, M. and Vega, V.B. and Wong, E. and Orlov, Y.L. and Zhang, W. and Jiang, J. and others},
  journal={Cell},
  volume={133},
  number={6},
  pages={1106--1117},
  year={2008},
  publisher={Elsevier}
}
v}

   The ChIP-seq data is described in the associated {{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE11431}GEO page}.
*)

(* #use "topfind" *)
(* #thread *)
(* #require "guizmin.bioinfo" *)

open Core.Std
open Guizmin_bioinfo
open Guizmin_bioinfo_labs

let sample x = Sra.(fastq_dump (sample x))

let sox2_fastq = List.map [ "SRR002023" (* ; "SRR002024" ; "SRR002025" "SRR002025" *) ] sample
let gfp_fastq = List.map [ "SRR001996" ; "SRR001997" ; "SRR001998" ; "SRR001999" ] sample

let bowtie_index_mm9 = Bowtie.index ~packed:true (Ucsc.genome_sequence `mm9)

let sox2_sam = Bowtie.align ~v:2 bowtie_index_mm9 sox2_fastq
let gfp_sam = Bowtie.align ~v:2 bowtie_index_mm9 gfp_fastq

let sox2_bam = Samtools.bam_of_sam sox2_sam
let gfp_bam = Samtools.bam_of_sam gfp_sam

let sox2_peaks = Macs.With_control.(
  run
    ~genome:`mm9
    ~pvalue:1e-5
    ~control:gfp_bam
    sox2_bam
  |! peaks
)

let sox2_motif = 
  Meme_suite.meme_chip 
    ~meme_nmotifs:3
    ~meme_minw:8
    ~meme_maxw:14
    (Ucsc.fasta_of_bed `mm9 (Bed.Named.make (Guizmin_table.remove_sharp_comments_and_header (Macs.best_peaks ~n:500 sox2_peaks))))

let wget_gzipped_bed url : Bed.Basic.file = Guizmin_unix.(
  gunzip (wget url)
)

let published_sox2_peaks =
  wget_gzipped_bed "http://www.ncbi.nlm.nih.gov/geosuppl/?acc=GSM288347&file=GSM288347%5FES%5FSox2%2Etxt%2Egz"

let () = Guizmin.(
  let base = default_base_directory () in
  let eval x = eval ~base ~np:1 x in
  let Dir p = eval sox2_motif in
  print_endline p
)


















