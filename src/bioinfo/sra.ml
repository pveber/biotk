open Core.Std
open Guizmin
open Guizmin_unix
open GzmUtils

type format

let sample id = 
  assert (String.length id = 9) ;
  let prefix = String.prefix id 6 in
  wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id)

let fastq_dump sra =
  f1
    ("guizmin.bioinfo.sra.fastq_dump[r1]",[])
    (fun env (File sra) path ->
      env.bash [ sp "fastq-dump -Z %s > %s" sra path ])
    sra
