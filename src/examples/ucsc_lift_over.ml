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
    
let mm9_peaks, unmapped_peaks =
  Ucsc.Lift_over.conversion
    (eval chain_file)
    (let File f = eval mm8_peaks in
     let mm8_peaks = In_channel.with_file f ~f:Peak.input in
     mm8_peaks # stream /@ (fun x -> x.Peak.loc))



















