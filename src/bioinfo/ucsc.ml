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
    "guizmin.bioinfo.ucsc.chromosome_sequences[1]"
    [ Param.string "org" org ]
    (fun env path ->
      env.bash [
	sp "mkdir -p %s" path;
	sp "cd %s" path ;
	sp "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org ;
	"gunzip *.gz"
      ])

let genome_sequence org = 
  f1
    "guizmin.bioinfo.ucsc.genome_sequence[1]" []
    (chromosome_sequences org)
    (fun env (Dir gp) path ->
      env.bash [ 
        "shopt -s nullglob" ;
        sp "cat %s/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > %s" gp path 
      ])


(* UGLY hack due to twoBitToFa: this tool requires that the 2bit
   sequence should be put in a file with extension 2bit. So I'm forced
   to create first a directory and then to select the unique file in it...*)
let genome_2bit_sequence_dir org = 
  let org = string_of_genome org in 
  d0
    "guizmin.bioinfo.ucsc.genome_sequence[1]"
    [ Param.string "org" org ]
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
    "guizmin.bioinfo.ucsc.fasta_of_bed[1]" []
    seq2b bed
    (fun env (File seq2b) (File bed) path ->
      sh "twoBitToFa -bed=%s %s %s" bed seq2b path)

module Chrom_info = struct
  type tabular data = {
    chrom : string ;
    chrom_length : int
  }
  type format
  type file = format Guizmin_table.file
end

let chrom_info_cmd1 org = Printf.sprintf "\
mysql --user=genome --host=genome-mysql.cse.ucsc.edu -A -N \
-e 'select chrom,size from chromInfo;' %s" (string_of_genome org)
let chrom_info_cmd2 path = Printf.sprintf "\
gawk -F'\t' '{printf \"%%s\\t%%s\\n\", $1,$2}' >> %s" path

let chrom_info org =
  f0
    "guizmin.bioinfo.ucsc.chrom_info[r1]"
    [ Param.string "org" (string_of_genome org) ]
    (fun env path ->
       let cmd =
         pipefail
           (chrom_info_cmd1 org)
           (chrom_info_cmd2 path)
       in
       ignore (Sys.command cmd))

let bedClip chrom_info bed =
  f2
    "guizmin.bioinfo.ucsc.bedClip[r1]" []
    chrom_info bed
    (fun env (File chrom_info) (File bed) path ->
       env.sh "bedClip -verbose=2 %s %s %s" bed chrom_info path)

let wig_of_bigWig bigWig = 
  f1
    "guizmin.bioinfo.ucsc.wig_of_bigWig[r1]" []
    bigWig
    (fun env (File bigWig) path ->
      env.bash [
        sp "bigWigToWig %s %s" bigWig path
      ]
    )

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
    | `position -> "-positions"
    in
    sprintf
      "liftOver %s %s %s %s %s"
      (string_of_output output) 
      old_locs chain_file new_locs unmapped_locs
    
  let conversion (File chain_file) xs =
    let old_locs_fn = create_input_file xs in
    let new_locs_fn = Filename.temp_file "gzm" ".locations"
    and unmapped_locs_fn = (* unmapped locations *)
      Filename.temp_file "gzm" ".locations" in
    let cmd = liftOver_cmd ~output:`position ~chain_file ~old_locs:old_locs_fn ~new_locs:new_locs_fn ~unmapped_locs:unmapped_locs_fn in
    sh "%s 2> /dev/null" cmd ;
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
      "guizmin.bioinfo.ucsc.bed_conversion[r2]" []
      chain_file bed
      (fun env (File chain_file) (File bed) path ->
        env.sh "mkdir -p %s" path ;
        env.sh "%s" (
          liftOver_cmd
            ~output:`bed
            ~chain_file 
            ~old_locs:bed
            ~new_locs:(path ^ "/mapped.bed") 
            ~unmapped_locs:(path ^ "/unmapped.bed")
        ))
          
  let mapped x = select x "mapped.bed"
  let unmapped x = select x "unmapped.bed"
end

module CustomTrack = struct
  open Printf

  type option = [ 
    `track_type of string
  | `name of string 
  | `description of string
  | `visibility of [ `hide | `full | `dense ]
  | `color of int * int * int
  | `altColor of int * int * int
  | `priority of int
  | `autoScale of bool
  | `gridDefault of bool
  | `maxHeightPixels of int * int * int
  | `graphType of [ `bar | `points ]
  | `viewLimits of [ `lower | `upper ]
  | `yLineMark of float 
  | `yLineOnOff of bool
  | `windowingFunction of [ `maximum | `mean | `minimum ]
  | `smoothingWindow of [ `off | `width of int ]
  | `dataUrl of string
  | `bigDataUrl of string
  | `useScore of bool
  ]

  let unparse_option buf = function
      `name n -> bprintf buf " name=\"%s\"" n
    | `track_type t -> bprintf buf " type=%s" t
    | `dataUrl u -> bprintf buf " dataUrl=%s" u
    | `bigDataUrl u -> bprintf buf " bigDataUrl=%s" u
    | `description d -> bprintf buf " description=\"%s\"" d
    | `visibility v -> bprintf buf " visibility=%s" 
	(match v with
	     `full -> "full"
	   | `dense -> "dense"
	   | `hide -> "hide")
    | `color (r,g,b) ->
	bprintf buf " color=%d,%d,%d" r g b 
    | `altColor (r,g,b) ->
	bprintf buf " altColor=%d,%d,%d" r g b 
    | `yLineOnOff b ->
	bprintf buf " yLineOnOff=%s" (if b then "on" else "off")
    | `priority p ->
	bprintf buf " priority=%d" p
    | `useScore b -> bprintf buf " useScore=%d" (if b then 1 else 0)
    | _ -> assert false

  let header_of_options opt =
    let buf = Buffer.create 1024 in 
      bprintf buf "track" ; 
      List.iter (unparse_option buf) opt ;
      Buffer.contents buf

  let url org track_options =
    let base = "http://genome.ucsc.edu/cgi-bin/hgTracks?db=" ^ (string_of_genome org) 
    and escaped_custom_text = Netencoding.Url.encode ~plus:false (header_of_options track_options) 
    in
      sprintf "%s&hgct_customText=%s" base escaped_custom_text
end
