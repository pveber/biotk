open Gg
open Core
open Misc
open Biotk_croquis

module type S = sig
  type t = private float array array
  val of_array : float array array -> t option
  val flat : int -> t
  val length : t -> int
  val composition : t -> float array
  val draw : ?palette:Color.t array -> t -> Croquis.t
  val draw_profile :
    ?palette:Gg.Color.t array ->
    float array ->
    Croquis.t
  val entropy : t -> float array
end

module Make(A : Alphabet.S) = struct
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

  let entropy = Array.map ~f:column_entropy

  let max_entropy = Float.log (float A.card) /. Float.log 2.

  let draw_y_scale max =
    let open Croquis in
    let text x y msg =
      text ~x ~y msg ~font:Croquis.Font.dejavu_sans_mono_bold ~size:0.1
    in
    group [
      line (0., 0.) (0., max) ;
      line (-0.1, 0.) (0., 0.) ;
      line (-0.1, max /. 2.) (0., max /. 2.) ;
      line (-. 0.1, max) (0., max) ;
      text (-. 0.2) 0. "0" ;
      text (-. 0.2) (max /. 2.) (Float.to_string (max /. 2.)) ;
      text (-. 0.2) max (Float.to_string max) ;
    ]

  let color_palette = function
    | None ->
      if A.card = 4 then
        Color.[| red ; blue ; v_srgbi 0xFF 0xB3 0 ; v_srgbi 0 0x80 0 |]
      else
        Croquis.Colormap.hsl ~lightness:0.5 ~saturation:1. A.card
    | Some p -> p

  let draw ?palette t =
    let color = color_palette palette in
    let open Croquis in
    let font = Font.dejavu_sans_mono_bold in
    let letter =
      List.map A.all ~f:(fun c ->
          sprintf "%c" (Char.uppercase (A.to_char c))
        )
      |> Array.of_list
    in
    let draw_letter ~x ~col ~sy l =
      if Core.Float.(sy < 1e-6) then None
      else
        Some (
          text ~valign:`base ~font l ~col ~x ~y:0.
          |> scale ~center:`origin ~sy
        )
    in
    let draw_col p_i =
      let entropy = column_entropy p_i in
      List.init A.card ~f:(fun j ->
          draw_letter letter.(j) ~col:color.(j) ~x:0. ~sy:p_i.(j)
        )
      |> List.filter_opt
      |> vstack ~align:`centered
      |> reshape
        ~bbox:(Box2.v (V2.v 0. 0.) (V2.v 1. (max_entropy -. entropy)))
    in
    Array.map t ~f:draw_col
    |> Array.to_list
    |> List.cons (draw_y_scale max_entropy)
    |> hstack ~align:`bottom

  let draw_profile ?palette xs =
    if Array.length xs <> A.card
    then invalid_argf "profile of length %d expected" A.card () ;
    let color = color_palette palette in
    if Array.length color <> A.card
    then invalid_argf "palette of length %d expected" A.card () ;
    let font = Croquis.Font.dejavu_sans_mono_bold in
    let symbols =
      List.mapi A.all ~f:(fun i symbol ->
          let unscaled =
            A.to_char symbol
            |> String.of_char
            |> Croquis.text ~font ~size:2. ~col:color.(i) ~x:0. ~y:0. in
          let bbox_unscaled = Croquis.bbox unscaled in
          let target_box =
            Box2.v
              (V2.v 0. (Box2.miny bbox_unscaled /. Box2.maxy bbox_unscaled))
              (V2.v (Box2.w bbox_unscaled /. 4.) xs.(i)) in
          let img =
            if Float.(xs.(i) < 1e-6) then Croquis.void target_box
            else Croquis.reshape unscaled ~bbox:target_box
          in
          img, Box2.w target_box
        )
      |> List.fold ~init:(0., []) ~f:(fun (pos_x, acc) (img, w) ->
          pos_x +. w, Croquis.translate ~dx:pos_x img :: acc
        )
      |> snd
      |> Croquis.group
    in
    Croquis.group [ draw_y_scale 1. ; symbols ]
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

module Protein = struct
  include Make(Amino_acid)

  (* Colors were taken from this publication :
     https://www.biorxiv.org/content/10.1101/2022.02.24.479670v1.full *)
  let dayhoff_palette = Color.[|
      v_srgb 1. 0.875 0.875 ;
      v_srgb 0.871 0.757 0.82 ;
      v_srgb 0.655 1. 0.953 ;
      v_srgb 0.482 1. 0.929 ;
      v_srgb 0.922 0.969 0.98 ;
      v_srgb 1. 0.749 0.749 ;
      v_srgb 1. 0.925 0.733 ;
      v_srgb 1. 0.882 0.745 ; (* Ile *)
      v_srgb 1. 0.851 0.471 ;
      v_srgb 1. 0.761 0.49 ;
      v_srgb 1. 0.643 0.235 ;
      v_srgb 0.31 1. 0.91 ;
      v_srgb 1. 0.502 0.502 ;
      v_srgb 0.137 1. 0.886 ;
      v_srgb 1. 0.812 0.337 ;
      v_srgb 1. 0.376 0.376 ;
      v_srgb 0.875 0. 0. ;
      v_srgb 1. 0.702 0.361 ;
      v_srgb 0.761 0.902 0.941 ;
      v_srgb 0.6 0.835 0.902 ;
    |]
end

let random_profile (module A : Alphabet.S) alpha rng =
  let alpha = Array.create ~len:A.card alpha in
  let theta = Array.create ~len:A.card 0. in
  Gsl.Randist.dirichlet rng ~alpha ~theta ;
  theta
