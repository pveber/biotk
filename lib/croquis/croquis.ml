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

let labeling_map l ~f =
  match l with
  | `C x -> `C (f x)
  | `A xs -> `A (Array.map xs ~f)

let labeling_map2_exn l1 l2 ~f =
  match l1, l2 with
  | `C x1, `C x2 -> `C (f x1 x2)
  | `C c, `A xs -> `A (Array.map xs ~f:(f c))
  | `A xs, `C c -> `A (Array.map xs ~f:(Fun.flip f c))
  | `A xs1, `A xs2 ->
    if Array.(length xs1 <> length xs2) then invalid_arg "array labelings with different lengths" ;
    `A (Array.map2_exn xs1 xs2 ~f)

type mark = Bullet | Circle

let normal_thickness = 0.01

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
end

type t =
  | Empty
  | Points of {
      bbox : Box2.t ;
      col : Color.t labeling ;
      mark : mark labeling ;
      thickness : float labeling ;
      x : float array ; (* inv: Array.(length x = length y *)
      y : float array ;
    }
  | Lines of {
      bbox : Box2.t ;
      col : Color.t labeling ;
      thickness : float ;
      x : float array ; (* inv: Array.(length x = length y *)
      y : float array ;
      arrow_head : Arrow_head.t option ;
    }

let empty = Empty

let box_convex_hull ~x ~y =
  let xmin = Float_array.min x in
  let xmax = Float_array.max x in
  let ymin = Float_array.min y in
  let ymax = Float_array.max y in
  Box2.of_pts (V2.v xmin ymin) (V2.v xmax ymax)

let points ?(col = `C Color.black) ?(mark = `C Bullet) ?(thickness = `C 0.01) ~x ~y () =
  if Array.(length x <> length y) then invalid_arg "x and y should have same length" ;
  let bbox = box_convex_hull ~x ~y in
  Points { col ; bbox ; mark ; thickness ; x ; y }

let lines ?(col = `C Color.black) ?(thickness = normal_thickness) ?(arrow_head = false) ~x ~y () =
  if Array.(length x <> length y) then invalid_arg "x and y should have same length" ;
  let n = Array.length x in
  if n < 2 then invalid_arg "at least two points expected" ;
  let arrow_head =
    if arrow_head then
      let _from_ = V2.v x.(n - 2) y.(n - 2) in
      let _to_   = V2.v x.(n - 1) y.(n - 1) in
      Some (Arrow_head.make _from_ _to_)
    else None
  in
  let line_bbox = box_convex_hull ~x ~y in
  let bbox = match arrow_head with
    | None -> line_bbox
    | Some ah -> Box2.union line_bbox (Arrow_head.bbox ah)
  in
  Lines { x ; y ; col ; bbox ; arrow_head ; thickness }

let image_of_lines ~x ~y ~col ~cap ~thickness ~arrow_head =
  let n = Array.length x in
  let path =
    ifold n ~init:P.empty ~f:(fun acc i ->
        P.line (V2.v x.(i) y.(i)) acc
      )
  in
  let area = `O { P.o with P.width = thickness ; cap } in
  let line_img = I.cut ~area path (I.const col) in
  match arrow_head with
  | None -> line_img
  | Some (ah : Arrow_head.t) ->
      let path =
        P.empty
        |> P.sub ah.tip
        |> P.line ah.wing_up
        |> P.line ah.wing_down
      in
      I.blend
        (I.cut ~area:`Anz path (I.const col))
        line_img

let path ?(col = Color.black) ?(thickness = `normal) ?(arrow_head = false) ?(cap = `Butt) points =
    let arrow_head = if arrow_head then arrow_head_geometry points else None in
    object
      method render =
        let body = match List.rev points with
          | [] | [ _ ] -> I.void
          | (ox, oy) :: (_ :: _ as t) ->
            let tip = match arrow_head with
              | None -> V2.v ox oy
              | Some h -> h#bottom
            in
            let path =
              List.fold t ~init:(P.empty |> P.sub tip) ~f:(fun acc (x, y) ->
                  P.line (V2.v x y) acc
                )
            in
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     cap } in
            I.cut ~area path (I.const col)
        and head = match arrow_head with
          | None -> I.void
          | Some head ->
            let path =
              P.empty
              |> P.sub head#tip
              |> P.line head#wing_up
              |> P.line head#wing_down
              in
              I.cut ~area:`Anz path (I.const col)
        in
        I.blend head body

      method bbox =
        let init = Box2.empty in
        let init =
          List.fold points ~init ~f:(fun acc (x, y) ->
              Box2.add_pt acc (V2.v x y)
            )
        in
        match arrow_head with
        | None -> init
        | Some head ->
          List.fold [ head#tip ; head#wing_up ; head#wing_down ] ~init ~f:Box2.add_pt
    end


let bbox = function
  | Empty -> Box2.empty
  | Points pts -> pts.bbox
  | Lines l -> l.bbox

let image_of_points ~mark ~col ~thickness ~x ~y =
  let area = labeling_map2_exn mark thickness ~f:(fun mark thickness ->
      match mark with
      | Bullet -> `Anz
      | Circle ->
        `O { P.o with P.width = thickness }
    )
  in
  let mark = labeling_map2_exn col area ~f:(fun col area ->
      I.cut ~area (P.empty |> P.circle V2.zero 0.1) (I.const col)
    )
  in
  ifold (Array.length x) ~init:I.void ~f:(fun acc i ->
      I.blend acc (I.move (V2.v x.(i) y.(i)) (labeling mark i))
    )

let image_of_croquis = function
  | Empty -> Vg.I.void
  | Points { mark ; col ; x ; y ; thickness ; _ } ->
    image_of_points ~mark ~col ~x ~y ~thickness
  | Lines { x ; y ; _ } ->
    image_of_lines ~x ~y

let render croquis file_format target =
  let view = bbox croquis in
  let size = Box2.size view in
  let image = image_of_croquis croquis in
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



type point = float * float


module Scaling = struct
  type 'a t = 'a -> float

  let id x = x
  let linear ~domain:(from_lo, from_hi) ~range:(to_lo, to_hi) =
    let delta = to_hi -. to_lo in
    let rho = delta /. (from_hi -. from_lo) in
    fun x -> (x -. from_lo) *. rho +. to_lo
end

module Viewport = struct
  type t = {
    scale_x : float -> float ;
    scale_y : float -> float ;
  }

  let linear ~xlim ~ylim ~size:(w, h) =
    { scale_x = Scaling.linear ~domain:xlim ~range:(0., w) ;
      scale_y = Scaling.linear ~domain:ylim ~range:(0., h) }

  let make ?(scale_x = Scaling.id) ?(scale_y = Scaling.id) () =
    { scale_x ; scale_y }

  let id = {
    scale_x = Fun.id ;
    scale_y = Fun.id ;
  }
  let scale_x vp = vp.scale_x
  let scale_y vp = vp.scale_y

  let scale vp (x, y) =
    (vp.scale_x x, vp.scale_y y)
end

type point_shape = [
  | `bullet
  | `circle
]


module Picture = struct
  class type t = object
    method render : image
    method bbox : Box2.t
  end

  type thickness = [
    | `normal
    | `thick
  ]

  let thickness_value = function
    | `thin -> 0.001
    | `normal -> 0.01
    | `thick -> 0.1

  let points ?(col = Color.black) ?(shape = `bullet) ~x ~y () =
    let xmin = Float_array.min x in
    let xmax = Float_array.max x in
    let ymin = Float_array.min y in
    let ymax = Float_array.max y in
    object
      method render =
        let area = match shape with
          | `bullet -> `Anz
          | `circle ->
            `O { P.o with P.width = thickness_value `thin }
        in
        let mark =
          I.cut ~area (P.empty |> P.circle V2.zero 0.1) (I.const col)
        in
        Array.map2_exn x y ~f:(fun x y ->
            I.move (V2.v x y) mark
          )
        |> Array.fold ~init:I.void ~f:I.blend

      method bbox =
        Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
    end

  let rect ?draw ?fill ?(thickness = `normal) ~xmin ~xmax ~ymin ~ymax () =
    object
      method render =
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
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     P.cap = `Square } in
            I.cut ~area p (I.const col)
        in
        let background = match fill with
          | None -> I.void
          | Some col ->
            I.cut ~area:`Anz p (I.const col)
        in
        I.blend outline background

      method bbox =
        Box2.v (V2.v xmin ymin) (V2.v (xmax -. xmin) (ymax -. ymin))
    end

  let circle ?draw ?fill ?(thickness = `normal) ~x ~y ~radius () =
    let c = V2.v x y in
    object
      method render =
        let p =
          P.empty
          |> P.circle c radius
        in
        let outline = match draw with
          | None -> I.void
          | Some col ->
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     P.cap = `Square } in
            I.cut ~area p (I.const col)
        in
        let background = match fill with
          | None -> I.void
          | Some col ->
            I.cut ~area:`Anz p (I.const col)
        in
        I.blend outline background

      method bbox =
        Box2.v_mid c (V2.v (2. *. radius) (2. *. radius))
    end

  let void =
    object
      method render = I.void
      method bbox = Box2.empty
    end

  let blend xs =
    object
      method render =
        List.fold xs ~init:I.void ~f:(fun acc p -> I.blend acc p#render)

      method bbox =
        List.map xs ~f:(fun x -> x#bbox)
        |> List.fold ~init:Box2.empty ~f:Box2.union
    end

  let blend2 x y = blend [ x ; y ]

  let bbox x = x#bbox

  let arrow_head_geometry points =
    match List.rev points with
    | [] | [ _ ] -> None
    | (x1, y1) :: (x2, y2) :: _ ->
      let tip = V2.v x1 y1 in
      let top = V2.v x2 y2 in
      let delta_colinear = V2.(sub top tip |> unit) in
      let delta_ortho = V2.(delta_colinear |> ortho |> smul 0.3) in
      let bottom = V2.(add tip delta_colinear) in
      let wing_up = V2.add bottom delta_ortho in
      let wing_down = V2.sub bottom delta_ortho in
      Some (
        object
          method bottom = bottom
          method tip = tip
          method wing_up = wing_up
          method wing_down = wing_down
        end
      )

  let path ?(col = Color.black) ?(thickness = `normal) ?(arrow_head = false) ?(cap = `Butt) points =
    let arrow_head = if arrow_head then arrow_head_geometry points else None in
    object
      method render =
        let body = match List.rev points with
          | [] | [ _ ] -> I.void
          | (ox, oy) :: (_ :: _ as t) ->
            let tip = match arrow_head with
              | None -> V2.v ox oy
              | Some h -> h#bottom
            in
            let path =
              List.fold t ~init:(P.empty |> P.sub tip) ~f:(fun acc (x, y) ->
                  P.line (V2.v x y) acc
                )
            in
            let area = `O { P.o with P.width = thickness_value thickness ;
                                     cap } in
            I.cut ~area path (I.const col)
        and head = match arrow_head with
          | None -> I.void
          | Some head ->
            let path =
              P.empty
              |> P.sub head#tip
              |> P.line head#wing_up
              |> P.line head#wing_down
              in
              I.cut ~area:`Anz path (I.const col)
        in
        I.blend head body

      method bbox =
        let init = Box2.empty in
        let init =
          List.fold points ~init ~f:(fun acc (x, y) ->
              Box2.add_pt acc (V2.v x y)
            )
        in
        match arrow_head with
        | None -> init
        | Some head ->
          List.fold [ head#tip ; head#wing_up ; head#wing_down ] ~init ~f:Box2.add_pt
    end

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
      | `top -> maxy
      | `bottom -> miny
    in
    object
      method render =
        I.move (V2.v (x -. dx) (y -. dy)) img

      method bbox =
        let bb = Box2.of_pts V2.(v 0. miny) V2.(v width maxy) in
        Box2.move (V2.v (x -. dx) (y -. dy)) bb
    end


  let translate ?(dx = 0.) ?(dy = 0.) t =
    object
      method bbox = Box2.move (V2.v dx dy) (t#bbox)
      method render =
        t#render
        |> I.move (V2.v dx dy)
    end

  let scale ?(center = `bbox_center) ?(sx = 1.) ?(sy = 1.) t =
    object
      method bbox =
        let bb = t#bbox in
        match center with
        | `bbox_center ->
           Box2.(v_mid (mid bb) (V2.v (w bb *. sx) (h bb *. sy)))
        | `origin ->
           Box2.of_pts
             (V2.v (Box2.minx bb *. sx) (Box2.maxy bb *. sy))
             (V2.v (Box2.maxx bb *. sx) (Box2.miny bb *. sy))
      method render =
        let center =
          match center with
          | `bbox_center -> Box2.mid t#bbox
          | `origin -> V2.zero
        in
        t#render
        |> I.move V2.(neg center)
        |> I.scale (V2.v sx sy)
        |> I.move center
    end

  let reshape t ~bbox =
    object
      method bbox = bbox

      method render =
        let src_bbox = t#bbox in
        let src_center = Box2.mid t#bbox in
        let dst_center = Box2.mid bbox in
        let sx = Box2.(w bbox /. w src_bbox) in
        let sy = Box2.(h bbox /. h src_bbox) in
        t#render
        |> I.move V2.(neg src_center)
        |> I.scale (V2.v sx sy)
        |> I.move dst_center
    end

  let path_of_box2 b =
    P.empty
    |> P.line (Box2.bl_pt b)
    |> P.line (Box2.br_pt b)
    |> P.line (Box2.tr_pt b)
    |> P.line (Box2.tl_pt b)
    |> P.line (Box2.bl_pt b)

  let crop t b =
    object
      method bbox = b
      method render =
        I.cut (path_of_box2 b) t#render
    end

  let frame t =
    object
      method bbox = t#bbox
      method render =
        let bb = t#bbox in
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
        let area = `O { P.o with P.width = 0.01 ;
                                 P.cap = `Square } in
        I.blend t#render (I.cut ~area p (I.const Color.black))
    end

  (* Tetris-like layout *)
  module Pileup_layout = struct
    type block = {
      bbox : Box2.t ;
      contents : t ;
    }

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

    let make_block contents = {
      bbox = contents#bbox ;
      contents ;
    }

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
                translate ~dy:(base_y -. Box2.miny bl.bbox) bl.contents
              )
          in
          loop (translated_layer :: acc) (base_y +. layer_height) rest
      in
      let sorted_blocks =
        List.map items ~f:make_block
        |> List.sort ~compare:(fun x y -> block_compare x y)
      in
      let layers =
        match sorted_blocks with
        | [] -> []
        | h :: _ -> loop [] (Box2.maxy h.bbox) sorted_blocks
      in
      List.concat layers
  end

  let pileup xs = blend (Pileup_layout.make xs)

  module VStack_layout = struct
    let make alignment items =
      let bboxes = List.map items ~f:(fun i -> i#bbox) in
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
    |> blend

  module HStack_layout = struct
    let make alignment items =
      let bboxes = List.map items ~f:(fun i -> i#bbox) in
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
    |> blend
end

module Plot = struct
  type t =
    | Points of {
        title : string option ;
        col : Color.t ;
        shape : point_shape ;
        x : float array ;
        y : float array ;
      }

  let points ?title ?(col = Color.black) ?(shape = `bullet) x y =
    Points { title ; col ; shape ; x ; y }

  let min_x = function
    | Points { x ; _ } -> Float_array.min x

  let max_x = function
    | Points { x ; _ } -> Float_array.max x

  let min_y = function
    | Points { y ; _ } -> Float_array.min y

  let max_y = function
    | Points { y ; _ } -> Float_array.max y

  let bb plot =
    let minx = min_x plot in
    let miny = min_y plot in
    Box2.v
      (V2.v minx miny)
      (V2.v (max_x plot -. minx) (max_y plot -. miny))

  let render ?(width = 10.) ?(height = 6.) plots =
    match plots with
    | [] -> Picture.void
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
      List.map plots ~f:(function
          | Points { x ; y ; col ; shape ; _ } ->
            let x = Array.map x ~f:(Viewport.scale_x vp) in
            let y = Array.map y ~f:(Viewport.scale_y vp) in
            Picture.points ~col ~x ~y ~shape ()
        )
      |> Picture.blend
end

type target = [
  | `File of string
  | `Channel of Stdlib.out_channel
  | `Buffer of Buffer.t
]

module Layout = struct
  type t =
    | Simple of Picture.t

  let simple x = Simple x

  let rec size ?width ?height view =
    let res w h = w *. 10., h *.10. in
    match width, height with
    | Some w, Some h -> res w h
    | Some w, None ->
      let h = Box2.h view *. w /. Box2.w view in
      res w h
    | None, Some h ->
      let w = Box2.w view *. h /. Box2.h view in
      res w h
    | None, None -> size ~width:10. view

  let box2_padding alpha b =
    let w = Box2.w b in
    let h = Box2.h b in
    let delta = Float.(min w h * alpha) in
    Box2.v_mid
      (Box2.mid b)
      (V2.v (w +. delta) (h +. delta))


  let render ?width ?height format (Simple pic) target =
    let view = box2_padding 0.01 pic#bbox in
    let size = size ?width ?height view in
    let image = pic#render in
    let renderer =
      match format with
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
      ignore (Vgr.render r (`Image (V2.of_tuple size, view, image))) ;
      ignore (Vgr.render r `End)
    in
    match target with
    | `File fn ->
      Out_channel.with_file fn ~f:(fun oc -> render (`Channel oc))
    | (`Channel _ | `Buffer _) as target -> render target
end
