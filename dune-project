(lang dune 1.11)
(generate_opam_files true)

(name gzt)
(source (github pveber/gzt))
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")
(homepage "https://github.com/pveber/gzt/")
(bug_reports "https://github.com/pveber/gzt/issues")

(package
  (name gzt)
  (synopsis "Another Bioinformatics Library")
  (description "gzt is a small library offering a few additional functionalities to biocaml")
  (depends
    "angstrom-unix"
    "biocaml"
    "core"
    "croquis"
    "fmt"
    "gsl"
    "ocamlnet"
    "owl"
    "pipes"
    "ppx_csv_conv"
    "ppx_deriving"))
