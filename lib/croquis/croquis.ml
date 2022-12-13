open Gg
open Vg
open Core

let ifold n ~init ~f =
  if n < 0 then invalid_arg "n should be positive" ;
  let rec loop i acc =
    if i = n then acc
    else loop (i + 1) (f acc i)
  in
  loop 0 init

module Float_array = struct
  let min xs =
    Array.fold xs ~init:Float.max_value ~f:Float.min
  let max xs =
    Array.fold xs ~init:Float.min_value ~f:Float.max
end

module Font = struct
  type t = Vg_text.Font.t Lazy.t

  let ascender x = Vg_text.Font.ascender (Lazy.force x)
  let descender x = Vg_text.Font.descender (Lazy.force x)
  let xmin x = Vg_text.Font.xmin (Lazy.force x)
  let xmax x = Vg_text.Font.xmax (Lazy.force x)
  let ymin x = Vg_text.Font.ymin (Lazy.force x)
  let ymax x = Vg_text.Font.ymax (Lazy.force x)

  let embedded_load fn =
    Lazy.from_fun (fun () ->
        let src = Stdlib.Option.get (Font_data.read fn) in
        match Vg_text.Font.load_from_string src with
        | Ok f -> f
        | Error (#Otfm.error as e) ->
          let buf = Buffer.create 253 in
          let fmt = Format.formatter_of_buffer buf in
          Otfm.pp_error fmt e ;
          Format.pp_print_flush fmt () ;
          failwith (Buffer.contents buf)
        | Error (`Read_error msg) ->
          failwithf "Read_error: %s" msg ()
      )

  let dejavu_sans_mono = embedded_load "DejaVuSansMono.ttf"
  let dejavu_sans_mono_bold = embedded_load "DejaVuSansMono-Bold.ttf"
  let dejavu_sans_mono_oblique = embedded_load "DejaVuSansMono-Oblique.ttf"
  let dejavu_sans_mono_bold_oblique = embedded_load "DejaVuSansMono-BoldOblique.ttf"

  let liberation_sans = embedded_load "LiberationSans-Regular.ttf"
  let liberation_sans_bold = embedded_load "LiberationSans-Bold.ttf"
  let liberation_sans_italic = embedded_load "LiberationSans-Italic.ttf"
  let liberation_sans_bold_italic = embedded_load "LiberationSans-BoldItalic.ttf"

  let default = liberation_sans
end

type 'a labeling = [`C of 'a | `A of 'a array]

let labeling l i =
  match l with
  | `C c -> c
  | `A xs -> xs.(i)

(* let labeling_map l ~f =
 *   match l with
 *   | `C x -> `C (f x)
 *   | `A xs -> `A (Array.map xs ~f) *)

let labeling_map2_exn l1 l2 ~f =
  match l1, l2 with
  | `C x1, `C x2 -> `C (f x1 x2)
  | `C c, `A xs -> `A (Array.map xs ~f:(f c))
  | `A xs, `C c -> `A (Array.map xs ~f:(Fun.flip f c))
  | `A xs1, `A xs2 ->
    if Array.(length xs1 <> length xs2) then invalid_arg "array labelings with different lengths" ;
    `A (Array.map2_exn xs1 xs2 ~f)

type mark = Bullet | Circle

let normal_thickness = 0.02

let box_convex_hull ~x ~y =
  let xmin = Float_array.min x in
  let xmax = Float_array.max x in
  let ymin = Float_array.min y in
  let ymax = Float_array.max y in
  Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax)

type t = {
  bbox : Box2.t ;
  img : image ;
}

let bbox { bbox ; _ } = bbox

module Points = struct
  type t =  {
    col : Color.t labeling ;
    mark : mark labeling ;
    thickness : float labeling ;
    x : float array ; (* inv: Array.(length x = length y *)
    y : float array ;
  }

  let v ?(col = `C Color.black) ?(mark = `C Bullet) ?(thickness = `C 0.01) ~x ~y () =
    if Array.(length x <> length y) then invalid_arg "x and y should have same length" ;
    { col ; mark ; thickness ; x ; y }

  let bbox { x ; y ; _ } = box_convex_hull ~x ~y

  let img { mark ; col ; thickness ; x ; y } =
    let area = labeling_map2_exn mark thickness ~f:(fun mark thickness ->
        match mark with
        | Bullet -> `Anz
        | Circle ->
          `O { P.o with P.width = thickness }
      )
    in
    let mark = labeling_map2_exn col area ~f:(fun col area ->
        I.cut ~area (P.empty |> P.circle V2.zero 0.05) (I.const col)
      )
    in
    ifold (Array.length x) ~init:I.void ~f:(fun acc i ->
        I.blend acc (I.move (V2.v x.(i) y.(i)) (labeling mark i))
      )

  let draw pts =
    let bbox = bbox pts in
    let img = img pts in
    { bbox ; img }
end

module Arrow_head = struct
  type t = {
    base : V2.t ;
    tip : V2.t ;
    wing_up : V2.t ;
    wing_down : V2.t ;
  }

  let make _from_ _to_ =
    let delta_colinear = V2.(sub _from_ _to_ |> unit) in
    let delta_ortho = V2.(delta_colinear |> ortho |> smul 0.3) in
    let base = V2.(add _to_ delta_colinear) in
    let wing_up = V2.add base delta_ortho in
    let wing_down = V2.sub base delta_ortho in
    { base ; wing_down ; wing_up ; tip = _to_ }

  let bbox { base ; tip ; wing_up ; wing_down } =
    Box2.(
      of_pts base tip
      |> Fun.flip add_pt wing_down
      |> Fun.flip add_pt wing_up
    )

  let render { tip ; wing_up ; wing_down ; _ } col =
    let path =
      P.empty
      |> P.sub tip
      |> P.line wing_up
      |> P.line wing_down
    in
    I.cut ~area:`Anz path (I.const col)

end

module Lines = struct
  type t = {
    col : Color.t ;
    thickness : float ;
    cap : P.cap ;
    x : float array ; (* inv: Array.(length x = length y *)
    y : float array ;
    maybe_arrow_head : Arrow_head.t option ;
  }

  let bbox { maybe_arrow_head ; x ; y ; _ } =
    let segment_bbox = box_convex_hull ~x ~y in
    match maybe_arrow_head with
    | None -> segment_bbox
    | Some ah -> Box2.union segment_bbox (Arrow_head.bbox ah)

  let v ?(col = Color.black) ?(thickness = normal_thickness) ?(cap = `Butt) ?(arrow_head = false) ~x ~y () =
    if Array.(length x <> length y) then invalid_arg "x and y should have same length" ;
    let n = Array.length x in
    if n < 2 then invalid_arg "at least two points expected" ;
    let maybe_arrow_head =
      if arrow_head then
        let _from_ = V2.v x.(n - 2) y.(n - 2) in
        let _to_   = V2.v x.(n - 1) y.(n - 1) in
        Some (Arrow_head.make _from_ _to_)
      else None
    in
    { x ; y ; col ; maybe_arrow_head ; thickness ; cap }

  let img { x ; y ; col ; cap ; thickness ; maybe_arrow_head ; _ } =
    let n = Array.length x in
    let path =
      if n > 0 then
        ifold (n - 1) ~init:(P.sub (V2.v x.(0) y.(0)) P.empty) ~f:(fun acc i ->
          P.line (V2.v x.(i + 1) y.(i + 1)) acc
          )
      else P.empty
    in
    let area = `O { P.o with P.width = thickness ; cap } in
    let line_img = I.cut ~area path (I.const col) in
    match maybe_arrow_head with
    | None -> line_img
    | Some ah ->
      I.blend (Arrow_head.render ah col) line_img

  let draw pts =
    let bbox = bbox pts in
    let img = img pts in
    { bbox ; img }
end

module Rect = struct
  type t = {
    xmin : float ; (* xmin <= xmax *)
    ymin : float ; (* ymin <= ymax *)
    xmax : float ;
    ymax : float ;
    draw : Color.t option ;
    fill : Color.t option ;
    thickness : float ;
  }

  let v ?draw ?fill ?(thickness = normal_thickness) ~xmin ~xmax ~ymin ~ymax () =
    if Float.(xmin > xmax || ymin > ymax) then invalid_arg "invalid coordinates" ;
    { xmin ; ymin ; xmax ; ymax ; draw ; fill ; thickness }

  let render { xmin ; ymin ; xmax ; ymax ; thickness ; draw ; fill } =
    let sw = V2.v xmin ymin in
    let nw = V2.v xmin ymax in
    let ne = V2.v xmax ymax in
    let se = V2.v xmax ymin in
    let p =
      P.empty
      |> P.sub sw
      |> P.line nw
      |> P.line ne
      |> P.line se
      |> P.line sw
    in
    let outline = match draw with
      | None -> I.void
      | Some col ->
        let area = `O { P.o with P.width = thickness ;
                                 P.cap = `Square } in
        I.cut ~area p (I.const col)
    in
    let background = match fill with
      | None -> I.void
      | Some col ->
        I.cut ~area:`Anz p (I.const col)
    in
    I.blend outline background

  let bbox { xmin ; ymin ; xmax ; ymax ; _ } =
    Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
end

module Circle = struct
  type t = {
    center : V2.t ;
    radius : float ;
    draw : Color.t option ;
    fill : Color.t option ;
    thickness : float ;
  }

  let v ?draw ?fill ?(thickness = normal_thickness) ~x ~y ~radius () =
    let center = V2.v x y in
    { center ; radius ; draw ; fill ; thickness }

  let render { center ; radius ; draw ; fill ; thickness } =
    let p =
      P.empty
      |> P.circle center radius
    in
    let outline = match draw with
      | None -> I.void
      | Some col ->
        let area = `O { P.o with P.width = thickness ;
                                 P.cap = `Square } in
        I.cut ~area p (I.const col)
    in
    let background = match fill with
      | None -> I.void
      | Some col ->
        I.cut ~area:`Anz p (I.const col)
    in
    I.blend outline background

  let bbox { center ; radius ; _ } =
    Box2.v_mid center (V2.v (2. *. radius) (2. *. radius))
end

let void bbox = {
  img = I.void ;
  bbox ;
}

let points ?col ?mark ?thickness ~x ~y () =
  Points.(draw @@ v ?col ?mark ?thickness ~x ~y ())

let lines ?col ?thickness ?arrow_head ?cap ~x ~y () =
  Lines.(draw @@ v ?col ?thickness ?arrow_head ?cap ~x ~y ())

let line ?col ?thickness ?arrow_head ?cap (x1, y1) (x2, y2) =
  Lines.(draw @@ v ?col ?thickness ?arrow_head ?cap ~x:[|x1;x2|] ~y:[|y1;y2|] ())

let rect ?draw ?fill ?thickness ~xmin ~xmax ~ymin ~ymax () =
  let r = Rect.v ?draw ?fill ?thickness ~xmin ~xmax ~ymin ~ymax () in
  let bbox = Rect.bbox r in
  let img = Rect.render r in
  { bbox ; img }

let circle ?draw ?fill ?thickness ~x ~y ~radius () =
  let c  = Circle.v ?draw ?fill ?thickness ~x ~y ~radius () in
  let bbox = Circle.bbox c in
  let img = Circle.render c in
  { bbox ; img }

let text ?(col = Color.black) ?(size = 12.) ?(font = Font.default) ?(halign = `middle) ?(valign = `base) ~x ~y text =
  let font = Lazy.force font in
  let layout = Vg_text.Layout.make font ~size text in
  let img = Vg_text.cut ~col:col layout in
  let width, maxy, miny = Vg_text.Layout.(width layout, maxy layout, miny layout) in
  let dx =
    match halign with
    | `middle -> width /. 2.
    | `left -> 0.
    | `right -> width
  in
  let dy =
    match valign with
    | `base -> 0.
    | `middle -> (miny +. maxy) /. 2.
    | `top -> maxy
    | `bottom -> miny
  in
  { img = I.move (V2.v (x -. dx) (y -. dy)) img ;
    bbox =
      let bb = Box2.of_pts V2.(v 0. miny) V2.(v width maxy) in
      Box2.move (V2.v (x -. dx) (y -. dy)) bb }

let group = function
  | [] -> void Box2.empty
  | h :: t as xs ->
    let img = List.fold t ~init:h.img ~f:(fun acc crq -> I.blend acc crq.img) in
    let bbox =
      List.map xs ~f:(fun x -> x.bbox)
      |> List.fold ~init:Box2.empty ~f:Box2.union
    in
    { bbox ; img }

let ( ++ ) a b = group [ a; b ]

let translate ?(dx = 0.) ?(dy = 0.) t = {
  bbox = Box2.move (V2.v dx dy) (t.bbox) ;
  img = I.move (V2.v dx dy) t.img ;
}

let scale ?(center = `bbox_center) ?(sx = 1.) ?(sy = 1.) t =
  let bbox =
    let bb = t.bbox in
    match center with
    | `bbox_center ->
      Box2.(v_mid (mid bb) (V2.v (w bb *. sx) (h bb *. sy)))
    | `origin ->
      Box2.of_pts
        (V2.v (Box2.minx bb *. sx) (Box2.maxy bb *. sy))
        (V2.v (Box2.maxx bb *. sx) (Box2.miny bb *. sy))
  and img =
    let center =
      match center with
      | `bbox_center -> Box2.mid t.bbox
      | `origin -> V2.zero
    in
    t.img
    |> I.move V2.(neg center)
    |> I.scale (V2.v sx sy)
    |> I.move center
  in
  { bbox ; img }

let reshape t ~bbox =
  let src_bbox = t.bbox in
  let src_center = Box2.mid t.bbox in
  let dst_center = Box2.mid bbox in
  let sx = Box2.(w bbox /. w src_bbox) in
  let sy = Box2.(h bbox /. h src_bbox) in
  let img =
    t.img
    |> I.move V2.(neg src_center)
    |> I.scale (V2.v sx sy)
    |> I.move dst_center
  in
  { bbox ; img }

module VStack_layout = struct
  let make alignment items =
    let bboxes = List.map items ~f:(fun i -> i.bbox) in
    let height = List.fold bboxes ~init:0. ~f:(fun acc bb ->
        acc +. Box2.h bb
      )
    in
    let justify y pic bbox =
      let dy = y -. Box2.maxy bbox in
      let dx = match alignment with
        | `none -> 0.
        | `centered -> -. Box2.midx bbox
        | `left -> -. Box2.minx bbox
        | `right -> -. Box2.maxx bbox
      in
      translate ~dx ~dy pic
    in
    List.fold2_exn items bboxes ~init:(height, []) ~f:(fun ((y, acc) as p) pic bbox ->
        if Box2.is_empty bbox then p
        else
          let pic' = justify y pic bbox in
          (y -. Box2.h bbox, pic' :: acc)
      )
    |> snd
    |> List.rev
end

let vstack ?(align = `none) xs =
  VStack_layout.make align xs
  |> group

module HStack_layout = struct
  let make alignment items =
    let bboxes = List.map items ~f:(fun i -> i.bbox) in
    let justify x pic bbox =
      let dx = x -. Box2.minx bbox in
      let dy = match alignment with
        | `none -> 0.
        | `centered -> -. Box2.midy bbox
        | `top -> -. Box2.maxy bbox
        | `bottom -> -. Box2.miny bbox
      in
      translate ~dx ~dy pic
    in
    List.fold2_exn items bboxes ~init:(0., []) ~f:(fun ((x, acc) as p) pic bbox ->
        if Box2.is_empty bbox then p
        else
          let pic' = justify x pic bbox in
          (x +. Box2.w bbox, pic' :: acc)
      )
    |> snd
    |> List.rev
end

let hstack ?(align = `none) xs =
  HStack_layout.make align xs
  |> group

let path_of_box2 b =
  P.empty
  |> P.line (Box2.bl_pt b)
  |> P.line (Box2.br_pt b)
  |> P.line (Box2.tr_pt b)
  |> P.line (Box2.tl_pt b)
  |> P.line (Box2.bl_pt b)

let crop t b =
  { bbox = b ; img = I.cut (path_of_box2 b) t.img }

let frame t =
  let img =
    let bb = t.bbox in
    let sw = Box2.bl_pt bb in
    let nw = Box2.tl_pt bb in
    let ne = Box2.tr_pt bb in
    let se = Box2.br_pt bb in
    let p =
      P.empty
      |> P.sub sw
      |> P.line nw
      |> P.line ne
      |> P.line se
      |> P.line sw
    in
    let area = `O { P.o with P.width = normal_thickness ;
                             P.cap = `Square } in
    I.blend t.img (I.cut ~area p (I.const Color.black))
  in
  { t with img }

(* Tetris-like layout *)
module Pileup_layout = struct
  let block_intersects b1 b2 =
    let open Float in
    let b1 = b1.bbox in
    let b2 = b2.bbox in
    Box2.(
      minx b1 <= minx b2 && minx b2 <= maxx b1
      || minx b2 <= minx b1 && minx b1 <= maxx b2
    )

  let block_compare b1 b2 =
    let b1 = b1.bbox in
    let b2 = b2.bbox in
    Caml.compare Box2.(minx b1, maxx b1) Box2.(minx b2, maxx b2)

  let x_overlap_partition = function
    | [] -> [], []
    | h :: t ->
      let rec loop inside outside last = function
        | [] -> List.rev (last :: inside), List.rev outside
        | h :: t ->
          if block_intersects last h then
            loop inside (h :: outside) last t
          else
            loop (last :: inside) outside h t
      in
      loop [] [] h t

  let make items =
    let rec loop acc base_y = function
      | [] -> List.rev acc
      | items ->
        let layer, rest = x_overlap_partition items in
        let layer_height =
          List.map layer ~f:(fun bl -> Box2.h bl.bbox)
          |> List.reduce_exn ~f:Float.max
        in
        let translated_layer =
          List.map layer ~f:(fun bl ->
              translate ~dy:(base_y -. Box2.miny bl.bbox) bl
            )
        in
        loop (translated_layer :: acc) (base_y +. layer_height) rest
    in
    let sorted_blocks =
      List.sort items ~compare:(fun x y -> block_compare x y)
    in
    let layers =
      match sorted_blocks with
      | [] -> []
      | h :: _ -> loop [] (Box2.maxy h.bbox) sorted_blocks
    in
    List.concat layers
end

let pileup xs = group (Pileup_layout.make xs)

let padding ?(delta = 0.1) ?(left = 0.) ?(right = 0.) ?(top = 0.) ?(bottom = 0.) x =
  let o = Box2.o x.bbox in
  let o' = V2.v (V2.x o -. delta -. left) (V2.y o -. delta -. top) in
  let w = Box2.w x.bbox in
  let w' = w +. 2. *. delta +. left +. right in
  let h = Box2.h x.bbox in
  let h' = h +. 2. *. delta +. top +. bottom in
  let bbox = Box2.v o' (V2.v w' h') in
  { x with bbox }

let box2_padding alpha b =
  let w = Box2.w b in
  let h = Box2.h b in
  let delta = Float.(min w h * alpha) in
  Box2.v_mid
    (Box2.mid b)
    (V2.v (w +. delta) (h +. delta))

type target = [
  | `File of string
  | `Channel of Stdlib.out_channel
  | `Buffer of Buffer.t
]

let render ?padding croquis file_format target =
  let view = match padding with
    | None -> croquis.bbox
    | Some space -> box2_padding space croquis.bbox in
  let size = Box2.size view in
  let image = croquis.img in
  let renderer =
    match file_format with
    | `pdf ->
      let otf_font x =
        Lazy.force x
        |> Vg_text.Font.data
        |> Vgr_pdf.otf_font
        |> function
        | Ok x -> x
        | Error _ -> assert false
      in
      let font (f : Vg.Font.t) =
        match f.name with
        | "DejaVuSansMono" -> otf_font Font.dejavu_sans_mono
        | "DejaVuSansMono-Bold" -> otf_font Font.dejavu_sans_mono_bold
        | "DejaVuSansMono-Oblique" -> otf_font Font.dejavu_sans_mono_oblique
        | "DejaVuSansMono-BoldOblique" -> otf_font Font.dejavu_sans_mono_bold_oblique
        | "LiberationSans" -> otf_font Font.liberation_sans
        | "LiberationSans-Bold" -> otf_font Font.liberation_sans_bold
        | "LiberationSans-Italic" -> otf_font Font.liberation_sans_italic
        | "LiberationSans-BoldItalic" -> otf_font Font.liberation_sans_bold_italic
        | _ -> `Sans
      in
      Vgr_pdf.target ~font ()
    | `svg -> Vgr_svg.target ()
  in
  let render target =
    let r = Vgr.create renderer target in
    ignore (Vgr.render r (`Image (size, view, image))) ;
    ignore (Vgr.render r `End)
  in
  match target with
  | `File fn ->
    Out_channel.with_file fn ~f:(fun oc -> render (`Channel oc))
  | (`Channel _ | `Buffer _) as target -> render target

let linear_scaling ~domain:(from_lo, from_hi) ~range:(to_lo, to_hi) =
  let delta = to_hi -. to_lo in
  let rho = delta /. (from_hi -. from_lo) in
  fun x -> (x -. from_lo) *. rho +. to_lo

module Axis = struct
  type t = {
    min : float ;
    max : float ;
    unit : float ;
    ticks : float list ;
  }

  let guess_unit lo hi =
    10. ** (Float.round ~dir:`Nearest (Float.log10 (hi -. lo)) -. 1.)

  let inferior_tick ~unit x =
    Float.round ~dir:`Down (x /. unit) *. unit

  let superior_tick ~unit x =
    Float.round ~dir:`Up (x /. unit) *. unit

  let make lo hi =
    let unit = guess_unit lo hi in
    let min = inferior_tick ~unit lo in
    let max = superior_tick ~unit hi in
    let ticks =
      Seq.unfold
        (fun i ->
           let x = min +. 2. *. float i *. unit in
           if Float.( <= ) x  max then Some (x, i + 1) else None)
        0
      |> Stdlib.List.of_seq
    in
    { min ; max ; unit ; ticks }

  let draw ax ~proj ~point ~text ~pos ~tick_length =
    let tick_pos = List.map ax.ticks ~f:proj in
    let pos' = pos -. tick_length in
    let area = `O { P.o with P.width = normal_thickness } in
    let ticks_img = List.fold tick_pos ~init:I.void ~f:(fun acc x ->
        let path =
          P.sub (point x pos') P.empty
          |> P.line (point x pos)
        in
        I.blend acc (I.cut ~area path (I.const Color.black))
      )
    in
    let ticks_bbox = Box2.of_pts (point (proj ax.min) pos') (point (proj ax.max) pos) in
    let ticks = { img = ticks_img ; bbox = ticks_bbox } in
    let labels =
      List.map2_exn ax.ticks tick_pos ~f:(fun v x ->
          text ~size:(tick_length *. 2.) x (pos -. 2. *. tick_length) (sprintf "%g" v)
        )
    in
    group (ticks :: labels)

end

module Viewport = struct
  type t = {
    scale_x : float -> float ;
    scale_y : float -> float ;
    visible_bbox : box2 ; (* visible area *)
    range_w : float ;
    range_h : float ;
    axis_x : Axis.t ;
    axis_y : Axis.t ;
  }

  let linear ~xlim ~ylim ~size:(w, h) =
    let domain_xmin, domain_xmax = xlim in
    let domain_ymin, domain_ymax = ylim in
    let axis_x = Axis.make domain_xmin domain_xmax in
    let axis_y = Axis.make domain_ymin domain_ymax in
    let rho = 0.05 in
    let visible_bbox =
      let xmin = Float.min domain_xmin axis_x.min in
      let xmax = Float.max domain_xmax axis_x.max in
      let ymin = Float.min domain_ymin axis_y.min in
      let ymax = Float.max domain_ymax axis_y.max in
      let w = xmax -. xmin in
      let h = ymax -. ymin in
      Box2.of_pts
        (V2.v (xmin -. rho *. w) (ymin -. rho *. h))
        (V2.v (xmax +. rho *. w) (ymax +. rho *. h))
    in
    { scale_x = linear_scaling ~domain:xlim ~range:(0., w) ;
      scale_y = linear_scaling ~domain:ylim ~range:(0., h) ;
      visible_bbox ;
      range_w = w ;
      range_h = h ;
      axis_x ; axis_y ;
    }

  let scale_x vp = vp.scale_x
  let scale_y vp = vp.scale_y

  let scale vp (x, y) = (scale_x vp x, scale_y vp y)

  let scale_v2 vp p =
    V2.v (scale_x vp (V2.x p)) (scale_y vp (V2.y p))

  let scale_box vp box =
    Box2.of_pts
      (scale_v2 vp (Box2.tl_pt box))
      (scale_v2 vp (Box2.br_pt box))
end

module Plot = struct
  type t =
    | Points of {
        title : string option ;
        col : Color.t ;
        mark : mark ;
        x : float array ;
        y : float array ;
      }

  type annotation =
    | ABLine of {
        col : Color.t ;
        thickness : float ;
        descr : [`H of float | `V of float | `AB of float * float]
      }

  let bb = function
    | Points { x ; y ; _ } ->
      let minx = Float_array.min x in
      let miny = Float_array.min y in
      let maxx = Float_array.max x in
      let maxy = Float_array.max y in
      Box2.v
        (V2.v minx miny)
        (V2.v (maxx -. minx) (maxy -. miny))

  let draw_axes (vp : Viewport.t) =
    let rho = 0.05 in
    let xmin = vp.scale_x (Box2.minx vp.visible_bbox) in
    let xmax = vp.scale_x (Box2.maxx vp.visible_bbox) in
    let ymin = vp.scale_y (Box2.miny vp.visible_bbox) in
    let ymax = vp.scale_y (Box2.maxy vp.visible_bbox) in
    let tick_length = ((vp.range_w *. rho /. 2.) +. (vp.range_h *. rho /. 2.)) /. 2. in
    let xticks = Axis.draw vp.axis_x ~proj:(Viewport.scale_x vp) ~point:V2.v ~text:(fun ~size x y msg -> text ~size ~valign:`top ~halign:`middle ~x ~y msg) ~pos:ymin ~tick_length in
    let yticks = Axis.draw vp.axis_y ~proj:(Viewport.scale_y vp) ~point:(Fun.flip V2.v) ~text:(fun ~size y x msg -> text ~size ~valign:`middle ~halign:`right ~x ~y msg) ~pos:xmin ~tick_length in
    group [
      rect ~draw:Color.black ~xmin ~xmax ~ymin ~ymax () ;
      xticks ; yticks ;
    ]

  let render_annotation (vp : Viewport.t) = function
    | ABLine { descr ; _ } ->
      let minx = Box2.minx vp.visible_bbox in
      let maxx = Box2.maxx vp.visible_bbox in
      let p1, p2 = match descr with
        | `H h ->
          (minx, h), (maxx, h)
        | `V v ->
          (v, vp.axis_y.min), (v, vp.axis_y.max)
        | `AB (a, b) -> (minx, a +. b *. minx), (maxx, a +. b *. maxx)
      in
      line (Viewport.scale vp p1) (Viewport.scale vp p2)

  let render ?(width = 10.) ?(height = 6.) ?(annotations = []) plots =
    match plots with
    | [] -> void Box2.empty
    | _ ->
      let bb =
        List.map plots ~f:bb
        |> List.reduce_exn ~f:Box2.union
      in
      let vp =
        Viewport.linear
          ~xlim:Box2.(minx bb, maxx bb)
          ~ylim:Box2.(miny bb, maxy bb)
          ~size:(width, height)
      in
      let plot_imgs =
        List.map plots ~f:(function
            | Points { x ; y ; col ; mark ; _ } ->
              let x = Array.map x ~f:(Viewport.scale_x vp) in
              let y = Array.map y ~f:(Viewport.scale_y vp) in
              points ~col:(`C col) ~x ~y ~mark:(`C mark) ()
          )
      and annotation_imgs =
        List.map annotations ~f:(render_annotation vp)
      in
      let img =
        plot_imgs @ annotation_imgs
        |> group
        |> Fn.flip crop (Viewport.scale_box vp vp.visible_bbox)
      in
      padding (draw_axes vp ++ img)

  let points ?title ?(col = Color.black) ?(mark = Bullet) x y =
    Points { title ; col ; mark ; x ; y }

  let hline ?(col = Color.black) ?(thickness = 1.) h =
    ABLine { descr = `H h ; thickness ; col }

  let vline ?(col = Color.black) ?(thickness = 1.) v =
    ABLine { descr = `V v ; thickness ; col }

  let abline ?(col = Color.black) ?(thickness = 1.) ~intercept ~slope () =
    ABLine { descr = `AB (intercept, slope) ; thickness ; col }
end

let plot ?width ?height ?annotations pl =
  Plot.render ?width ?height ?annotations pl

module Colormap = struct
  type t = Color.t array

  let greys n =
    Array.init n ~f:(fun i ->
        Color.gray (float i /. float n)
      )

  let check_interval label value lo hi =
    if Float.(lo > value || value > hi) then
      invalid_argf "Argument %s should lie between %g and %g" label lo hi ()

  (* The formula are taken from https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB *)
  let color_of_hsl ~h ~s ~l =
    check_interval "h" h 0. 360. ;
    check_interval "s" s 0. 1. ;
    check_interval "l" l 0. 1. ;
    let chroma = (1. -. Float.abs (2. *. l -. 1.)) *. s in
    let h' = h /. 60. in
    let x = chroma *. (1. -. Float.abs (Float.(h' % 2.) -. 1.)) in
    let r1, g1, b1 =
      if Float.(h' < 1.) then (chroma, x, 0.)
      else if Float.(h' < 2.) then (x, chroma, 0.)
      else if Float.(h' < 3.) then (0., chroma, x)
      else if Float.(h' < 4.) then (0., x, chroma)
      else if Float.(h' < 5.) then (x, 0., chroma)
      else if Float.(h' < 6.) then (chroma, 0., x)
      else assert false
    in
    let m = l -. chroma /. 2. in
    Color.v (r1 +. m) (g1 +. m) (b1 +. m) 1.

  let hsl ~saturation:s ~lightness:l n =
    Array.init n ~f:(fun i ->
        let h = float i /. float n *. 360. in
        color_of_hsl ~h ~s ~l
      )
end

let palette xs =
  Array.map xs ~f:(fun col ->
      rect ~fill:col ~xmin:0. ~xmax:1. ~ymin:0. ~ymax:1. ()
      |> frame
      |> padding ~delta:0.3
    )
  |> Array.to_list
  |> hstack
