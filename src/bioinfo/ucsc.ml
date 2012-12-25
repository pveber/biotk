open GzmUtils
open GzmCore

type bigWig
type wig
type genome = [ `mm8 | `mm9 | `hg18 | `hg19 | `sacCer2 ]

let string_of_genome = function
|  `mm8 -> "mm8"
|  `mm9 -> "mm9"
| `hg18 -> "hg18"
| `hg19 -> "hg19"
| `sacCer2 -> "sacCer2"

let chromosome_sequences org =
  let org = string_of_genome org in 
  d0
    ("guizmin.bioinfo.ucsc.chromosome_sequences[1]", [ Param.string "org" org ])
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
      env.bash [ 
        "shopt -s nullglob" ;
        sp "cat %s/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > %s" gp path 
      ])
    (chromosome_sequences org)


(* UGLY hack due to twoBitToFa: this tool requires that the 2bit
   sequence should be put in a file with extension 2bit. So I'm forced
   to create first a directory and then to select the unique file in it...*)
let genome_2bit_sequence_dir org = 
  let org = string_of_genome org in 
  d0
    ("guizmin.bioinfo.ucsc.genome_sequence[1]", [ Param.string "org" org ])
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

module Lift_over = struct
  open Printf
  open Core.Std
  open Biocaml_stream.Infix
  open MBSchema

  type chain_file = [`lift_over_chain] file

  let chain_file ~org_from ~org_to =
    let org_from = string_of_genome org_from
    and org_to = string_of_genome org_to in
    let url = 
      sprintf 
        "ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/liftOver/%sTo%s.over.chain.gz" 
        org_from org_from (String.capitalize org_to)
    in
    Guizmin_unix.(gunzip (wget url))

  let create_input_file xs =
    let fn = Filename.temp_file "gzm" ".locations" in
    xs /@ Location.to_string
    |! lines_to_file fn ;
    fn

  let liftOver_cmd ~output ~chain_file ~old_locs ~new_locs ~unmapped_locs = 
    let string_of_output = function
    | `bed -> ""
    | `position -> "-position"
    in
    sh
      "liftOver %s %s %s %s %s 2> /dev/null"
      (string_of_output output) 
      old_locs chain_file new_locs unmapped_locs
    
  let conversion (File chain_file) xs =
    let old_locs_fn = create_input_file xs in
    let new_locs_fn = Filename.temp_file "gzm" ".locations"
    and unmapped_locs_fn = (* unmapped locations *)
      Filename.temp_file "gzm" ".locations" in
    liftOver_cmd ~output:`position ~chain_file ~old_locs:old_locs_fn ~new_locs:new_locs_fn ~unmapped_locs:unmapped_locs_fn ;
    let new_locs = 
      List.map (lines_of_file new_locs_fn) ~f:Location.of_string
    and unmp_locs =
      List.map (lines_of_file unmapped_locs_fn) ~f:Location.of_string
    in
    sh "rm -f %s %s %s liftOver_*" old_locs_fn new_locs_fn unmapped_locs_fn ;
    new_locs, unmp_locs

  let bed_conversion ~org_from ~org_to bed =
    let chain_file = chain_file ~org_from ~org_to in
    d2
      ("guizmin.bioinfo.ucsc.bed_conversion[r2]", [])
      (fun env (File chain_file) (File bed) path ->
        sh "mkdir -p %s" path ;
        liftOver_cmd
          ~output:`bed
          ~chain_file 
          ~old_locs:bed
          ~new_locs:(path ^ "/mapped.bed") 
          ~unmapped_locs:(path ^ "/unmapped.bed"))
      chain_file bed

  let mapped x = select x "mapped.bed"
  let unmapped x = select x "unmapped.bed"
end



















