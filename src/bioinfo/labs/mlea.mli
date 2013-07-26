open Guizmin
open Guizmin_bioinfo
open Guizmin_bioinfo.MBSchema

type result = {
  motif_id : string ;
  pwm : Biocaml_pwm.t ;
  positions : int array ;
  fold : float ;
  pval : float ;
  pval_adj : float ;
  fdr : float
}

val jaspar_library : 
  Biocaml.Jaspar.motif list pipeline -> 
  (string * Biocaml_pwm.t) list pipeline

val graph : 
  Ucsc.genome ->
  'a Bed.With_name.file ->
  Biocaml_pwm.t pipeline ->
  [`pdf] file

val test :
  Ucsc.genome ->
  'a Bed.With_name.file ->
  (string * Biocaml_pwm.t) list pipeline ->
  result list pipeline

module Tsv_output : sig
  type tabular line = {
    motif_id : string ;
    fold : float ;
    pval : float ;
    pval_adj : float ;
    fdr : float
  }

  val of_test : result list pipeline -> line Guizmin_table.file
end

val latex_output : result list pipeline -> [`latex] file




















