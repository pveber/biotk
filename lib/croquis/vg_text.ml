(* code adapted from vecho.ml in vg library *)
(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Result

let str = Printf.sprintf
(* let otfm_err_str err = *)
(*   Format.fprintf Format.str_formatter "%a" Otfm.pp_error err; *)
(*   Format.flush_str_formatter () *)

let ( >>= ) x f = match x with
  | Error _ as e -> e
  | Ok v -> f v

(* Font information *)

module Int = struct type t = Vg.glyph let compare = compare end
module Gmap = Map.Make (Int) (* glyph maps *)
module Cmap = Gmap           (* uchar maps *)

module Font = struct
  type t = {
    font_name : string ;
    raw : string;                                      (* The font bytes. *)
    cmap : int Cmap.t;           (* Maps unicode scalar values to glyphs. *)
    advs : int Gmap.t;             (* Maps glyph to advances in em space. *)
    kern : int Gmap.t Gmap.t;    (* Maps glyph pairs to kern adjustement. *)
    head : Otfm.head ;
    hhea : Otfm.hhea ;
    units_per_em : int;                        (* Number of units per em. *)
  }

  let name fi = fi.font_name
  let data fi = fi.raw

  let ascender fi = float fi.hhea.hhea_ascender /. float fi.units_per_em
  let descender fi = float fi.hhea.hhea_descender /. float fi.units_per_em
  let xmin fi = float fi.head.head_xmin /. float fi.units_per_em
  let ymin fi = float fi.head.head_ymin /. float fi.units_per_em
  let xmax fi = float fi.head.head_xmax /. float fi.units_per_em
  let ymax fi = float fi.head.head_ymax /. float fi.units_per_em

  let string_of_file inf =
    try
      let ic = if inf = "-" then stdin else open_in_bin inf in
      let close ic = if inf <> "-" then close_in ic else () in
      let buf_size = 65536 in
      let b = Buffer.create buf_size in
      let s = Bytes.create buf_size in
      try
        while true do
          let c = input ic s 0 buf_size in
          if c = 0 then raise Exit else
            Buffer.add_subbytes b s 0 c
        done;
        assert false
      with
      | Exit -> close ic; Ok (Buffer.contents b)
      | Failure _ -> close ic; Error (`Read_error (str "%s: input file too large" inf))
      | Sys_error e -> close ic; Error (`Read_error (str "%s: %s" inf e))
    with
    | Sys_error e -> Error (`Read_error (str "%s: %s" inf e))

  let add_adv acc g adv _ = Gmap.add g adv acc
  let add_cmap acc kind (u0, u1) g =
    let acc = ref acc in
    begin match kind with
      | `Glyph_range ->
        for i = 0 to (u1 - u0) do acc := Cmap.add (u0 + i) (g + i) !acc done;
      | `Glyph ->
        for u = u0 to u1 do acc := Cmap.add u g !acc done
    end;
    !acc

  let add_ktable acc i =
    (if i.Otfm.kern_dir = `H && i.Otfm.kern_kind = `Kern then `Fold else `Skip),
    acc

  let add_kpair acc g0 g1 kv =
    let m = try Gmap.find g0 acc with Not_found -> Gmap.empty in
    Gmap.add g0 (Gmap.add g1 kv m) acc

  let load_from_string raw =
    let d = Otfm.decoder (`String raw) in
    let r =
      Otfm.postscript_name d                      >>= fun font_name ->
      Otfm.head d                                 >>= fun head ->
      Otfm.cmap d add_cmap Cmap.empty             >>= fun (_, cmap) ->
      Otfm.hmtx d add_adv Gmap.empty              >>= fun advs ->
      Otfm.kern d add_ktable add_kpair Gmap.empty >>= fun kern ->
      Otfm.hhea d                                 >>= fun hhea ->
      let font_name = match font_name with None -> "Unknown" | Some n -> n in
      let units_per_em = head.Otfm.head_units_per_em in
      Ok { font_name ; raw; cmap; advs; kern; hhea ; units_per_em ; head }
    in
    (r : (_, Otfm.error) result :> (_, [> Otfm.error]) result)

  let load_from_file fn =
    match string_of_file fn with
    | Error _ as e -> e
    | Ok raw -> load_from_string raw
end

let get_glyph fi g = try Gmap.find g fi.Font.cmap with Not_found -> 0
let get_adv fi g = try Gmap.find g fi.Font.advs with Not_found -> 0
let get_kern fi g g' =
  try Gmap.find g' (Gmap.find g fi.Font.kern) with Not_found -> 0

let layout fi ~font_size:size text =
  let u_to_em = float fi.Font.units_per_em in
  let rec add (prev, gs, advs, kerns as acc) i = function
  | `Malformed _ -> add acc i (`Uchar Uutf.u_rep)
  | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      let advs = get_adv fi g :: advs in
      let kerns = if prev = -1 then kerns else (get_kern fi prev g) :: kerns in
      (g, g :: gs, advs, kerns)
  in
  let rec advances acc len advs kerns = match advs, kerns with
  | adv :: advs, k :: kerns ->
      let adv = adv + k in
      let sadv = V2.v ((size *. (float adv)) /. u_to_em) 0. in
      advances (sadv :: acc) (len + adv) advs kerns
  | adv :: [], [] -> acc, len + adv
  | _ -> assert false
  in
  let _, gs, advs, kerns = Uutf.String.fold_utf_8 add (-1, [], [], []) text in
  let advs, len = advances [] 0 (List.rev advs) (List.rev kerns) in
  List.rev gs, List.rev advs, ((size *. float len) /. u_to_em)

let glyphs_of_string fi text =
  let f acc _ = function
    | `Malformed _ -> get_glyph fi (Uchar.to_int Uutf.u_rep) :: acc
    | `Uchar u ->
      let g = get_glyph fi (Uchar.to_int u) in
      g :: acc
  in
  Uutf.String.fold_utf_8 f [] text
  |> List.rev

let text_length fi ~font_size text =
  let _glyphs_rev, _advances_rev, len = layout fi ~font_size text in
  len

let cut ?(col = Color.black) ?(size = 12.) font text =
  let vg_font = { Vg.Font.name = Font.name font ;
                  slant = `Normal;
                  weight = `W400;
                  size } in
  let base = size *. Font.descender font in
  let height = size *. Font.ascender font -. base in
  let glyphs, advances, width = layout font ~font_size:size text in
  let i =
    Vg.I.const col |>
    Vg.I.cut_glyphs ~text ~advances vg_font glyphs
  in
  i, Box2.v (V2.v 0. base) (Size2.v width height)

let bbox ~size font text =
  let under = size *. Font.ymin font in
  let above = size *. Font.ymax font in
  let _, _, width = layout font ~font_size:size text in
  Box2.v (V2.v 0. under) (Size2.v width (above -. under))
