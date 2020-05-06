open Core_kernel
open Misc
open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw : t -> Croquis.Picture.t
end

module type Alphabet = sig
  type t
  val all : t list
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) = struct
  type t = float array array

  let length = Array.length

  let flat n =
    let eps = 1. /. float A.card in
    Array.init n ~f:(fun _ ->
        Array.create ~len:A.card eps
      )

  let of_array = function
    | [||] -> None
    | xs ->
      if Array.for_all xs ~f:(fun x -> Array.length x = A.card)
      then Some xs
      else None

  let composition profile =
    let weights = Array.init A.card ~f:(fun j ->
        sum (Array.length profile) ~f:(fun i -> profile.(i).(j))
      ) in
    let total = Array.fold weights ~init:0. ~f:( +. ) in
    Array.map weights ~f:(fun w -> w /. total)

  let xlnx x =
    if Float.(x = 0.) then 0. else x *. Float.log x /. Float.log 2.

  let column_entropy p =
    -. sum (Array.length p) ~f:(fun j -> xlnx p.(j))

  let draw_y_scale () =
    let open Croquis.Picture in
    let text x y msg =
      text ~x ~y msg ~font:Croquis.Font.free_sans_bold ~size:0.1
    in
    blend [
      path [ 0., 0. ; 0., 2. ] ;
      path [ -. 0.1, 0. ; 0., 0. ] ;
      path [ -. 0.1, 1. ; 0., 1. ] ;
      path [ -. 0.1, 2. ; 0., 2. ] ;
      text (-. 0.2) 0. "0" ;
      text (-. 0.2) 1. "1" ;
      text (-. 0.2) 2. "2" ;
    ]

  let draw t =
    let open Croquis in
    let open Picture in
    let open Gg in
    let size = 10. in
    let font = Font.free_sans_bold in
    let letter =
      List.map A.all ~f:(fun c ->
          sprintf "%c" (Char.uppercase (A.to_char c))
        )
      |> Array.of_list
    in
    let color =
      if A.card = 4 then
        Color.[| red ; blue ; v_srgbi 0xFF 0xB3 0 ; v_srgbi 0 0x80 0 |]
      else
        Array.init A.card ~f:(fun i -> Color.gray (float i *. 127. /. float A.card))
    in
    let left = 3.5 and right = 3.5 in
    let draw_letter ~x ~y ~col ~sy l =
      if Core_kernel.Float.(sy < 1e-6) then None
      else
        let xmin = x -. left in
        let ymin = y -. Font.(ymax font +. ymin font) *. size /. 2. *. 1.1 in
        let xmax = x +. right in
        let ymax = 0.425 *. size *. Font.ymax font in
        let bbox = Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax) in
        Some (
          (* blend2 *)
            (crop (text ~size ~font l ~col ~x ~y) bbox)
            (* (rect ~draw:Color.black ~xmin ~xmax ~ymin ~ymax ()) *)
          |> scale ~sy
        )
    in
    let draw_col p_i =
      List.init A.card ~f:(fun j ->
          draw_letter letter.(j) ~col:color.(j) ~x:0. ~y:0. ~sy:p_i.(j)
        )
      |> List.filter_opt
      |> vstack ~align:`centered
    in
    let entropy = Array.map ~f:column_entropy t in
    let min_entropy = Array.reduce_exn entropy ~f:Core_kernel.Float.min in
    let logo =
      Array.map t ~f:draw_col
      |> Array.to_list
      |> List.mapi ~f:(fun i pic ->
          reshape ~bbox:(Box2.v (V2.v 0. 0.) (V2.v 1. (2. -. entropy.(i)))) pic
        )
      |> hstack ~align:`bottom
      |> reshape ~bbox:(Box2.(v V2.zero V2.(v (float (Array.length t)) (2. -. min_entropy))))
    in
    blend2 (draw_y_scale ()) logo
end

module DNA = struct
  include Make(Nucleotide)

  let reverse_complement mat =
    let open Nucleotide in
    let n = Array.length mat in
    let f i c = mat.(n - i - 1).(to_int c) in
    Array.init n ~f:(fun i ->
        Nucleotide.[| f i t ; f i g ; f i c ; f i a |]
      )
end
