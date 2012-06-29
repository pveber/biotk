open GzmUtils
open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]
let string_of_genome = function
    `mm9 -> "mm9"
  | `hg18 -> "hg18"
  | `sacCer2 -> "sacCer2"

let golden_path org =
  let org = string_of_genome org in 
  d0
    ("guizmin/ucsc-goldenPath", [ string "org" org ])
    (fun path ->
      bash [
	sp "mkdir -p %s/chromosomes" path;
	sp "cd %s/chromosomes" path ;
	sp "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org ;
	"gunzip *.gz"
      ])

let genome_sequence org = 
  f1
    ("guizmin/ucsc-genome-sequence", [])
    (fun (Dir gp) path ->
      bash [ sp "cat %s/chromosomes/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > %s" gp path ])
    (golden_path org)
