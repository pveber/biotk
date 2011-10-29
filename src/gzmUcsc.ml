open Printf
open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]

let string_of_genome = function
    `mm9 -> "mm9"
  | `hg18 -> "hg18"
  | `sacCer2 -> "sacCer2"

let genome_chromosome_sequences org = factory
  ~recipe:[ 
    "mkdir -p $@_tmp" ;
    "cd $@_tmp" ;
    sprintf "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" (string_of_genome org) ;
    "gunzip *.gz" ;
    "mv "
  ]
  ~deps:[]

    
let genome_sequence org = 
  let chrz = genome_chromosome_sequences org in
  target 
    [ sp "bash -c 'cat %s/{chr?.fa,chr??.fa,chr???.fa,chr????.fa} > $@'" (path chrz) ]
    ([] ++ chrz)
