(lang dune 1.11)
(generate_opam_files true)

(name biotk)
(source (github pveber/biotk))
(license CeCILL-B)
(authors "Philippe Veber")
(maintainers "philippe.veber@gmail.com")
(homepage "https://github.com/pveber/biotk/")
(bug_reports "https://github.com/pveber/biotk/issues")

(package
  (name biotk)
  (synopsis "Bioinformatics toolkit")
  (description "biotk implements various functionalities in
                computational biology, like parsers/unparsers for some
                file formats, a few basic data structures and sequence
                algorithms, as well as access to some databases.")
  (depends
    "angstrom-unix"
    "biocaml"
    ("core" (>= "v0.14.0"))
    "fmt"
    "ocamlify"
    "ocamlnet"
    "otfm"
    "ppx_csv_conv"
    "ppx_deriving"
    "vg"
    "tyxml"))
