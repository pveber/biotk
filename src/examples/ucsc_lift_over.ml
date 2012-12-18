open Core.Std
open Biocaml_stream.Infix
open Guizmin
open Guizmin_bioinfo
open Guizmin_bioinfo.MBSchema

type table peak = {
  loc : Location ;
}

let url = "ftp://ftp.ncbi.nlm.nih.gov/pub/geo/DATA/supplementary/samples/GSM288nnn/GSM288345/GSM288345_ES_Nanog.txt.gz"

let mm8_peaks : Peak.table file = Guizmin_unix.(gunzip (wget url))

let chain_file = 
  Ucsc.Lift_over.chain_file ~org_from:`mm8 ~org_to:`mm9
    
let load_table x =
  let File f = eval x in
  In_channel.with_file f ~f:Peak.table_of_channel

let mm9_peaks, unmapped_peaks =
  Ucsc.Lift_over.conversion
    (eval chain_file)
    (let mm8_peaks = load_table mm8_peaks in
     mm8_peaks # stream /@ (fun x -> x.Peak.loc))

let () =
  print_endline "Report" ;
  print_endline "======" ;
  printf "Number of input regions: %d\n" (load_table mm8_peaks)#length ;
  printf "Number of mapped regions: %d\n" (List.length mm9_peaks) ;
  printf "Number of unmapped regions: %d\n" (List.length unmapped_peaks) ;
  printf "First mapped regions:\n" ;
  List.take mm9_peaks 10
  |! List.iter ~f:(fun l -> print_endline (Location.to_string l))




















