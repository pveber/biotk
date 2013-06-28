open Core.Std
open Printf
open Guizmin

module Stream = Biocaml_stream

type species = [
| `homo_sapiens
| `mus_musculus
]
type genome = [
| `hg19
| `mm9
]


let reference_genome ~release ~species =
  match species with
  | `mus_musculus when 63 <= release && release <= 65 -> `mm9
  | `homo_sapiens when release = 71 -> `hg19
  | _ -> failwith "unknown release for this species" 

(* acronym of the lab where the species was sequenced *)
let lab_label_of_genome = function
| `hg19 -> "GRCh37"
| `mm9 -> "NCBIM37"
| `mm10 -> "GRCm38"

let string_of_species = function
| `homo_sapiens -> "homo_sapiens"
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

type gtf = Gtf.file

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


module Annotation = struct
  open MBSchema

  let exon_item = Biocaml_gff.(
    function 
    | `record ({ feature = Some "exon" } as it) -> Some it
    | _ -> None
  )

  let attribute id it = Biocaml_gff.(
    List.Assoc.find_exn it.attributes id |! List.hd_exn
  )
  let transcript_id = attribute "transcript_id"
  let gene_id = attribute "gene_id"
  let exon_number it = attribute "exon_number" it |! int_of_string
  let location { Biocaml_gff.pos = (lo,hi) ; seqname } =
    seqname, Biocaml_range.make lo hi

  let compare_exons e1 e2 =
    compare (exon_number e1) (exon_number e2)

  let transcripts gtf =
    v1
      "guizmin.bioinfo.ensembl.annotation.transcripts[r1]"
      []
      gtf
      (
        fun env gtf ->
          Gtf.with_file ~tags:[`version `two] gtf ~f:(fun items ->
            Stream.filter_map items ~f:exon_item
            |! Stream.group ~f:transcript_id
            |! Stream.map ~f:Biocaml_gff.(fun items ->
              match Stream.to_list items with
              | [] -> assert false (* not possible to build empty groups *)
              | h :: _ as l -> {
                Transcript.id = transcript_id h ;
                gene_id = gene_id h ;
                strand = (
                  match h.Biocaml_gff.strand with
                  | `plus -> `sense
                  | `minus -> `antisense
                  | _ -> assert false (* shouldn't happen in an ensembl gtf *)
                ) ;
                exons = (
                  let sorted_exons = List.sort compare_exons l in
                  List.map sorted_exons ~f:location
                )
              }
            )
            |! Stream.to_list
          )
      )

  let genes gtf =
    v1
      "guizmin.bioinfo.ensembl.annotation.genes[r1]"
      []
      (transcripts gtf)
      (
        fun env transcripts ->
          let gene_make (id, transcripts) = {
            Gene.id ; aliases = [] ; transcripts
          } 
          in
          Stream.of_list transcripts
          |! Stream.map ~f:(fun x -> x.Transcript.gene_id, x)
          |! Biocaml_accu.relation 
          |! Stream.map ~f:gene_make
          |! Stream.to_list
      )

end



















