open Biocaml
open Guizmin
open GzmUtils

module J = Biocaml_jaspar

type db

let mirror = d0
  "guizmin.bioinfo.jaspar.mirror" []
  (fun env path ->
    env.bash [
      sp "mkdir -p %s" path ;
      sp "cd %s" path ;
      "wget http://jaspar.genereg.net/html/DOWNLOAD/Archive.zip" ;
      "unzip Archive.zip" ;
      "rm Archive.zip" ;
    ])

let core = v1
  "guizmin.bioinfo.jaspar.core" []
  mirror
  (fun env (Dir mirror) ->
    Biocaml.Jaspar.load (sp "%s/jaspar_CORE/non_redundant/all_species/FlatFileDir" mirror))

let core_vertebrates = v1
  "guizmin.bioinfo.jaspar.core_vertebrates" []
  mirror
  (fun env (Dir mirror) ->
    Biocaml.Jaspar.load (sp "%s/jaspar_CORE/non_redundant/by_tax_group/vertebrates/FlatFileDir/" mirror))

let cne = v1
  "guizmin.bioinfo.jaspar.cne" []
  mirror
  (fun env (Dir mirror) ->
    List.filter
      (fun m -> m.J.collection = J.CNE)
      (J.load (sp "%s/all_data/FlatFileDir" mirror)))



















