open Printf

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

let gtf ~release ~species =
  let url = 
    sprintf "ftp://ftp.ensembl.org/pub/release-%d/gtf/%s/%s.%s.%d.gtf.gz"
      release (string_of_species species) 
      (String.capitalize (string_of_species species))
      (lab_label_of_genome (reference_genome ~release ~species)) release
  in
  Guizmin_unix.(gunzip (wget url))


















