open GzmUtils
open GzmCore

type bigWig
type wig
type genome = [ `mm9 | `hg18 | `hg19 | `sacCer2 ]

let string_of_genome = function
    `mm9 -> "mm9"
  | `hg18 -> "hg18"
  | `hg19 -> "hg19"
  | `sacCer2 -> "sacCer2"

let chromosome_sequences org =
  let org = string_of_genome org in 
  d0
    ("guizmin.bioinfo.ucsc.chromosome_sequences[1]", [ string "org" org ])
    (fun env path ->
      env.bash [
	sp "mkdir -p %s" path;
	sp "cd %s" path ;
	sp "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org ;
	"gunzip *.gz"
      ])

let genome_sequence org = 
  f1
    ("guizmin.bioinfo.ucsc.genome_sequence[1]", [])
    (fun env (Dir gp) path ->
      env.bash [ sp "cat %s/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > %s" gp path ])
    (chromosome_sequences org)


(* UGLY hack due to twoBitToFa: this tool requires that the 2bit
   sequence should be put in a file with extension 2bit. So I'm forced
   to create first a directory and then to select the unique file in it...*)
let genome_2bit_sequence_dir org = 
  let org = string_of_genome org in 
  d0
    ("guizmin.bioinfo.ucsc.genome_sequence[1]", [ string "org" org ])
    (fun env path ->
      env.bash [
        sp "mkdir %s" path ;
        sp "cd %s" path ;
        sp "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/bigZips/%s.2bit" org org
      ])

let genome_2bit_sequence org = 
  select (genome_2bit_sequence_dir org) ((string_of_genome org) ^ ".2bit")



let wg_encode_crg_mappability n org =
  let url = sp "ftp://hgdownload.cse.ucsc.edu/gbdb/%s/bbi/wgEncodeCrgMapabilityAlign%dmer.bigWig" (string_of_genome org) n in
  Guizmin_unix.wget url

let wg_encode_crg_mappability_36 org = wg_encode_crg_mappability 36 org
let wg_encode_crg_mappability_40 org = wg_encode_crg_mappability 40 org
let wg_encode_crg_mappability_50 org = wg_encode_crg_mappability 50 org
let wg_encode_crg_mappability_75 org = wg_encode_crg_mappability 75 org
let wg_encode_crg_mappability_100 org = wg_encode_crg_mappability 100 org

let fasta_of_bed org bed = 
  let seq2b = genome_2bit_sequence org in
  f2
    ("guizmin.bioinfo.ucsc.fasta_of_bed[1]", [])
    (fun env (File seq2b) (File bed) path ->
      sh "twoBitToFa -bed=%s %s %s" bed seq2b path)
    seq2b bed

let wig_of_bigWig bigWig = 
  f1
    ("guizmin.bioinfo.ucsc.wig_of_bigWig[r1]", [])
    (fun env (File bigWig) path ->
      env.bash [
        sp "bigWigToWig %s %s" bigWig path
      ]
    )
    bigWig










