open Core.Std
open Guizmin
open Guizmin_unix
open GzmUtils

type format

let sample id = 
  if (String.length id = 9) then (
    let prefix = String.prefix id 6 in
    wget (sprintf "ftp://ftp-trace.ncbi.nlm.nih.gov/sra/sra-instant/reads/ByRun/sra/SRR/%s/%s/%s.sra" prefix id id)
  )
  else failwithf "Guizmin_bioinfo.Sra.sample: id %s is invalid (not 9 characters long)" id ()
    
let fastq_dump sra =
  f1
    "guizmin.bioinfo.sra.fastq_dump[r1]" []
    sra
    (fun env (File sra) path ->
      env.bash [ sp "fastq-dump -Z %s > %s" sra path ])













