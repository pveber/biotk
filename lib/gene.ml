open Base

type t = {
  chr : string ;
  strand : [ `Plus | `Minus ] ;
  exons : (int * int) list ;
}

let for_all_same_chr = function
  | [] -> Or_error.error_string "empty gene"
  | { GLoc.chr ; _ } :: t ->
    if List.for_all t ~f:(fun l -> String.(l.chr = chr))
    then Ok chr
    else Or_error.error_string "Not all exons are from the same chromosome"

let comparison_of_strand = function
  | `Plus -> GLoc.strictly_before
  | `Minus -> Fn.flip GLoc.strictly_before

let rec list_fold_consecutive_pairs xs ~init ~f = match xs with
  | [] | [ _ ] -> init
  | e1 :: (e2 :: _ as t) ->
    f (list_fold_consecutive_pairs t ~f ~init) e1 e2

let exon_strict_order compare xs =
  let ordered =
    list_fold_consecutive_pairs xs ~init:true ~f:(fun b e1 e2 ->
        b && compare e1 e2
      )
  in
  if ordered then Ok ()
  else Or_error.error_string "Exons not ordered or overlaping"

let make ~strand ~exons =
  let open Or_error.Monad_infix in
  for_all_same_chr exons >>= fun chr ->
  exon_strict_order (comparison_of_strand strand) exons >>= fun () ->
  Ok {
    chr ;
    strand ;
    exons = List.map exons ~f:(fun r -> r.lo, r.hi)
  }

let space_between (s1, e1) (s2, e2) =
  if e1 <= s2 then (e1, s2)
  else e2, s1

let introns g =
  list_fold_consecutive_pairs g.exons ~init:[] ~f:(fun acc e1 e2 ->
      let lo, hi = space_between e1 e2 in
      { GLoc.chr = g.chr ; lo ; hi } :: acc
    )

let exons g =
  List.map g.exons ~f:(fun (lo, hi) -> GLoc.{ chr = g.chr ; lo ; hi })
