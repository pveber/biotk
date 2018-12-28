open Base

type t = {
  chr : string ;
  lo : int ;
  hi : int ;
} [@@deriving compare, sexp]

let to_string { chr ; lo ; hi } =
  Printf.sprintf "%s:%d-%d" chr lo hi

let of_string_exn s =
  try Caml.Scanf.sscanf s "%[^:]:%d-%d" (fun chr lo hi -> { chr ; lo ; hi })
  with _ -> failwith ("GLoc.of_string_exn: " ^ s)

let%test "of_string_exn_1" =
  Caml.(
    of_string_exn "chr1:3053032-3053034"
    =
    { chr = "chr1" ; lo = 3053032 ; hi = 3053034 }
  )

let of_string s =
  try Ok (of_string_exn s)
  with _ -> Error `Parse_error

let range { lo ; hi ; _ } = Range.make ~lo ~hi

let strictly_before x y =
  match String.compare x.chr y.chr with
  | -1 -> true
  |  1 -> false
  |  0 -> x.hi < y.lo
  | _ -> assert false

let%test "strictly_before_1" =
  strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "b" ; lo = 0 ; hi = 4 }

let%test "strictly_before_2" =
  strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 10 ; hi = 40 }

let%test "strictly_before_3" =
  not (
    strictly_before { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 4 ; hi = 40 }
  )

let intersects x y =
  String.(x.chr = y.chr)
  && (
    (x.lo <= y.lo && y.lo <= x.hi)
    || (y.lo <= x.lo && x.lo <= y.hi)
  )

let%test "intersects_1" =
  intersects { chr = "a" ; lo = 0 ; hi = 4 } { chr = "a" ; lo = 2 ; hi = 30 }
