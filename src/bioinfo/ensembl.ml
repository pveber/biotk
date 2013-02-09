open Printf
open Guizmin

type species = [
| `mus_musculus
]
type genome = [
| `mm9
]


let reference_genome ~release ~species =
  match species with
  | `mus_musculus when 63 <= release && release <= 65 -> `mm9
  | _ -> failwith "unknown release for this species" 

(* acronym of the lab where the species was sequenced *)
let lab_label_of_genome = function
| `mm9 -> "NCBIM37"
| `mm10 -> "GRCm38"

let string_of_species = function
| `mus_musculus -> "mus_musculus"

let ucsc_chr_names_gtf gtf =
  f1
    "guizmin.bioinfo.ensembl.ucsc_chr_names_gtf[r1]"
    []
    gtf
    (
      fun env (File gtf) path ->
        env.sh "gawk '{print \"chr\" $0}' %s | sed 's/chrMT/chrM/g' > %s" gtf path
    )

let gtf ?(chr_name = `ensembl) ~release ~species =
  let url = 
    sprintf "ftp://ftp.ensembl.org/pub/release-%d/gtf/%s/%s.%s.%d.gtf.gz"
      release (string_of_species species) 
      (String.capitalize (string_of_species species))
      (lab_label_of_genome (reference_genome ~release ~species)) release
  in
  let gtf = Guizmin_unix.(gunzip (wget url)) in
  match chr_name with
  | `ensembl -> gtf
  | `ucsc -> ucsc_chr_names_gtf gtf




















