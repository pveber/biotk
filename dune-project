(lang dune 2.7)
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
    ("angstrom" (>= 0.15.0))
    ("angstrom-unix" (>= 0.15.0))
    "binning"
    ("camlzip" (>= 1.05))
    ("core" (>= "v0.15.0"))
    ("core_bench" :with-test)
    ("core_kernel" (>= "v0.15.0"))
    ("core_unix" (>= "v0.15.0"))
    (crunch (>= 3.2.0))
    "fmt"
    "gsl"
    (ocaml (>= 4.11.0))
    "otfm"
    "ppx_csv_conv"
    "ppx_deriving"
    ("result" (>= 1.5))
    ("rresult" (>= 0.6.0))
    ("uri" (>= 4.2.0))
    "vg"
    ("tyxml" (>= 4.4.0))
    ("zlib" (>= 0.6)))
    )
