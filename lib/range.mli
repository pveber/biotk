(** Ranges of contiguous integers (integer intervals).

    A range is a contiguous sequence of integers from a lower bound to
    an (excluded) upper bound.

    Note: this module is adapted from biocaml 0.8
 *)

type t = private { lo : int; hi : int } [@@deriving compare, sexp]
(** Type of a range. *)

val make_exn : lo:int -> hi:int -> t
(** [make_exn ~lo ~hi] builds the range [\[lo ; hi\[]. In particular
    the range is empty if [lo = hi]. Raises [Invalid_argument] if [lo
    > hi]. *)

val size : t -> int
(** [size v] returns the number of integers in [v], i.e. [v.hi - v.lo]. *)

val member : t -> int -> bool
(** [member t k] returns true if [t] contains [k]. *)

val to_string : t -> string
(** String representation of an range, intended only for human
    legibility. *)

val to_list : t -> int list
(** [to_list v] returns the set of integers contained in [v], in
    ascending order. *)

val overlap : t -> t -> int
(** [overlap u v] returns amount of overlap between two ranges. A
    positive value indicates number of integers common to [u] and
    [v]. A negative value indicates the number of integers in between
    non-overlapping ranges. A zero value means the ranges are exactly
    adjacent to each other. The relation is symmetric. *)

(** {6 Set Operations} *)

val union : t -> t -> [ `Joint of t | `Disjoint of t * t ]
(** [union u v] returns the range(s) representing the union of [u] and
    [v]. If [u] and [v] overlap, their union can be represented as a
    single range. If not, their union is a disjoint combination of two
    ranges. *)

val intersect : t -> t -> t option
(** [intersect u v] returns the range representing the intersection of
    [u] and [v]. Return None if intersection is empty. *)

(** {6 Positional Range}
    Positional means an range is viewed as coming either before or
    after another.
*)

val before : t -> t -> bool
(** [before u v] is true if [strict_before u v || equal u v]. *)

val after : t -> t -> bool
(** [after u v] is equivalent to [before v u]. *)

val strict_before : t -> t -> bool
(** [strict_before u v] is true if [u.lo < v.lo && u.hi < v.hi]. *)

val strict_after : t -> t -> bool
(** [strict_after u v] is equivalent to [strict_before v u]. *)

val compare_positional : t -> t -> int option
(** [compare_positional u v] returns -1 if [u] is strictly before [v],
    0 if [u] is equal to [v], +1 if [u] is strictly after [v], and returns
    None otherwise. *)

(** {6 Containment Range}
    Containment means a range is viewed as being inside, or a subset
    of, another.
*)

val subset : t -> t -> bool
(** [subset u v] is true if [u] is a subset of [v]. *)

val superset : t -> t -> bool
(** [superset u v] is true if [u] is a superset of [v]. *)

val strict_subset : t -> t -> bool
(** [strict_subset u v] is true if [u] is a strict subset of [v]. *)

val strict_superset : t -> t -> bool
(** [strict_superset u v] is true if [u] is a strict superset of
    [v]. *)

val compare_containment : t -> t -> int option
(** [compare_containment u v] returns -1 if [u] is a strict subset of
    [v], 0 if [u] is equal to [v], +1 if [u] is a strict superset of [v],
    and returns None otherwise. *)

val relative_position : t -> wrt:t -> [
    | `Before
    | `Before_with_intersection
    | `Included
    | `Equal
    | `Contains
    | `After_with_intersection
    | `After
  ]

val convex_hull : t -> t -> t
