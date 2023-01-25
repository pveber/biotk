open Core

type t = { lo : int; hi : int }
[@@deriving compare, sexp]

let make_exn ~lo ~hi =
  if lo <= hi then { lo; hi }
  else invalid_argf "lower bound %d larger than upper bound %d" lo hi ()

let size v = v.hi - v.lo
let equal u v = compare u v = 0
let member t k = t.lo <= k && k < t.hi

let to_string t =
  String.concat [ "["; string_of_int t.lo; ", "; string_of_int t.hi; "[" ]

let to_list v = List.init (size v) ~f:(( + ) v.lo)
let overlap u v = min u.hi v.hi - max u.lo v.lo

let union u v =
  if overlap u v < 0 then `Disjoint (u, v)
  else `Joint { lo = min u.lo v.lo; hi = max u.hi v.hi }

let intersect u v =
  let l = max u.lo v.lo in
  let h = min u.hi v.hi in
  if l <= h then Some { lo = l; hi = h } else None

let strict_before u v = u.lo < v.lo && u.hi < v.hi
let strict_after u v = strict_before v u
let before u v = strict_before u v || equal u v
let after u v = before v u

let compare_positional u v =
  if equal u v then Some 0
  else if strict_before u v then Some (-1)
  else if strict_after u v then Some 1
  else None

let subset u v = u.lo >= v.lo && u.hi <= v.hi
let superset u v = subset v u
let strict_subset u v = subset u v && not (equal u v)
let strict_superset u v = strict_subset v u

let compare_containment u v =
  if equal u v then Some 0
  else if strict_subset u v then Some (-1)
  else if strict_superset u v then Some 1
  else None

let relative_position x ~wrt:y =
  if x.lo < y.lo then (
    if x.hi < y.lo then `Before
    else if x.hi < y.hi then `Before_with_intersection
    else `Contains
  )
  else if x.lo = y.lo then (
    if x.hi < y.hi then `Included
    else if x.hi = y.hi then `Equal
    else `Contains
  )
  else if x.lo <= y.hi then (
    if x.hi <= y.hi then `Included
      else `After_with_intersection
    )
  else `After

let convex_hull p q =
  { lo = min p.lo q.lo ;
    hi = max p.hi q.hi }
