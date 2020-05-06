open Biotk_croquis

type tree =
  | Leaf of { text : string }
  | Node of { children : branch list }
and branch = Branch of { length : float ; tip : tree }

val leaf : string -> tree
val bnode : branch -> branch -> tree
val node : branch list -> tree
val branch : float -> tree -> branch

val draw_tree : tree -> Croquis.Picture.t
val draw_branch : branch -> Croquis.Picture.t
