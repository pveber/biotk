open GzmCore

type genome = [ `mm9 | `hg18 | `sacCer2 ]
let string_of_genome = function
    `mm9 -> "mm9"
  | `hg18 -> "hg18"
  | `sacCer2 -> "sacCer2"

let golden_path org =
  let org = string_of_genome org in 
  let path = "guizmin/goldenPath/" ^ org in
  step
    ~body:[
      sp "mkdir -p %s/chromosomes" path;
      sp "cd %s/chromosomes" path ;
      sp "wget ftp://hgdownload.cse.ucsc.edu/goldenPath/%s/chromosomes/*" org ;
      "gunzip *.gz" ;
      sp "cat {chr?.fa,chr??.fa,chr???.fa,chr????.fa} > ../%s.fa" org
    ]
    path


let genome_sequence org = 
  select (sp "%s.fa" (string_of_genome org)) (golden_path org)
