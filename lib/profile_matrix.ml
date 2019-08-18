open Core_kernel
open Misc

module type S = sig
  type t = private float array array
  val length : t -> int
  val random : ?alpha:float -> int -> t
  val simulate_sequence : t -> string
  val composition : t -> float array
  val draw : t -> Croquis.Picture.t
end

module type Alphabet = sig
  type t
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) = struct
  type t = float array array

  let length = Array.length

  let random ?(alpha = 1.) motif_length =
    let alpha = Array.init 4 ~f:(fun _ -> Random.float alpha) in
    Array.init motif_length ~f:(fun _ ->
        Owl.Stats.dirichlet_rvs ~alpha
      )

  let simulate_sequence eps =
    String.init (Array.length eps) ~f:(fun i ->
        Dna_sequence.random_base eps.(i)
      )

  let composition profile =
    let weights = Array.init 4 ~f:(fun j ->
        sum (Array.length profile) ~f:(fun i -> profile.(i).(j))
      ) in
    let total = Owl.Stats.sum weights in
    Array.map weights ~f:(fun w -> w /. total)

  let xlnx x =
    if x = 0. then 0. else x *. Float.log x

  let column_entropy p =
    sum (Array.length p) ~f:(fun j -> -. xlnx p.(j))

  let draw t =
    let open Croquis in
    let open Picture in
    let open Gg in
    let size = 10. in
    let font = Font.free_sans_bold in
    let delta = 0.86 *. size *. (Font.ascender font) in
    let letter = [|"A";"C";"G";"T"|] in
    let color = Color.[| red ; blue ; v_srgbi 0xFF 0xB3 0 ; v_srgbi 0 0x80 0 |] in
    let left = 3.5 and right = 3.4 in
    let draw_letter ~x ~y ~col ~sy l =
      if sy < 1e-6 then None
      else
        let xmin = x -. left in
        let ymin = y -. 0.075 *. delta in
        let xmax = x +. right in
        let ymax = y +. 0.925 *. delta in
        let bbox = Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax) in
        Some (
          crop (text ~size ~font l ~col ~x ~y) bbox
          |> scale ~sy
        )
    in
    let draw_col p_i =
      List.init 4 ~f:(fun j ->
          draw_letter letter.(j) ~col:color.(j) ~x:0. ~y:0. ~sy:p_i.(j)
        )
      |> List.filter_opt
      |> vstack ~align:`centered
    in
    Array.map t ~f:draw_col
    |> Array.to_list
    |> List.mapi ~f:(fun i pic ->
        let ent = column_entropy t.(i) in
        scale ~sy:(2. -. ent) pic
      )
    |> hstack ~align:`bottom
end

module DNA = Make(Nucleotide)
