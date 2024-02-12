(** b ibed format

  [ibed] files are produced by {{:https://doi.org/10.1186/s13059-016-0992-2}CHiCAGO} software
  for detecting statistically significant interaction events in Capture HiC data.
  Each row represents a chromosomal interaction.
  You can check what such a file looks like {{:https://www.bioconductor.org/packages/devel/bioc/vignettes/Chicago/inst/doc/Chicago.html#ibed-format-ends-with-ibed} here}.
  This module offers a representation of this kind of file (essentially a list of records)
  and some functions to handle this type.

  example:
  {[
    open Biotk

    let () =
      Ibed.from_file "data/pchic/mouse.ibed"
        |> List.filter (fun i -> i.bait.chr = "chr13")
        |> Ibed.to_file "data/pchic/mouse_chr13.ibed"
  ]}
*)

type item = {
  bait_names: string list;
  bait: GLoc.t;
  other_names: string list;
  other: GLoc.t;
  n_reads: int;
  score: float;
}

type t = item list

(** Sort contacts by the genomic locaction of bait part, ascending by default. *)
val sort : ?desc:bool -> t -> t

(** Produce a list of contacts list, each sublist contains all contact that share the same bait. *)
val group_by_bait : t -> (GLoc.t * t) list

(** Produce a list of tuple, first element of the couple is the chromosome, the second is all contacts
    that belong to this chromosome. *)
val group_by_chr : t -> (string * t) list

val from_file_exn : ?header:bool -> string -> t

val to_file : ?header:bool -> string -> t -> unit
