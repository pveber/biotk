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

open Guizmin_bioinfo

let sample x = Sra.(fastq_dump (sample x))

let sox2_fastq = List.map sample [ "SRR002023" (* ; "SRR002024" ; "SRR002025" "SRR002025" *) ]
let gfp_fastq = List.map sample [ "SRR001996" ; "SRR001997" ; "SRR001998" ; "SRR001999" ]

let bowtie_index_mm9 = Bowtie.index ~packed:true (Ucsc.genome_sequence `mm9)

let sox2_bam = Bowtie.align ~v:2 bowtie_index_mm9 sox2_fastq
let gfp_bam = Bowtie.align ~v:2 bowtie_index_mm9 gfp_fastq

(* let sox2_peaks = Macs.With_control.( *)
(*   run *)
(*     ~genome:`mm9 *)
(*     ~pvalue:1e-5 *)
(*     ~control:gfp_bam *)
(*     sox2_bam *)
(* ) *)

let wget_gzipped_bed url : Bed.Minimal.file = Guizmin_unix.(
  gunzip (wget url)
)

let published_sox2_peaks =
  wget_gzipped_bed "http://www.ncbi.nlm.nih.gov/geosuppl/?acc=GSM288347&file=GSM288347%5FES%5FSox2%2Etxt%2Egz"

let () = Guizmin.(
  let base = default_base_directory () in
  let eval x = eval ~base ~np:1 x in
  let File p = eval (sox2_bam) in
  print_endline p
)


















