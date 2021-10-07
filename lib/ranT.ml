let find_map lo hi ~f =
  let rec loop i =
    if i = hi then None
    else
      match f i with
      | None -> loop (i + 1)
      | Some y -> Some y
  in
  loop lo

let iter a b ~f =
  let rec loop i =
    if i < b then (
      f i ;
      loop (i + 1) )
  in
  loop a

let for_all a b ~f =
  let rec loop i = i = b || (f i && loop (i + 1)) in
  loop a

let%test "for_all 1..4 <= 10" = for_all 1 4 ~f:(( >= ) 10)

let%test "not (for_all 1..4 <= 2)" =
  not @@ for_all 1 4 ~f:(( >= ) 2)

let%test "not (for_all 1..4 <= 0)" =
  not @@ for_all 1 4 ~f:(( >= ) 2)

let find a b ~f =
  let rec loop i =
    if i < b then if f i then Some i else loop (i + 1) else None
  in
  loop a

let count a b ~f =
  let rec loop acc i =
    if i < b then loop (if f i then acc + 1 else acc) (i + 1)
    else acc
  in
  loop 0 a

let fold a b ~init ~f =
  let rec loop acc i = if i >= b then acc else loop (f acc i) (i + 1) in
  loop init a
