open Gg
open Vg

module Font : sig
  type t

  val name : t -> string
  val data : t -> string
  val ascender : t -> float
  val descender : t -> float
  val xmin : t -> float
  val ymin : t -> float
  val xmax : t -> float
  val ymax : t -> float

  val load_from_string :
    string ->
    (t, [> Otfm.error | `Read_error of string]) result

  val load_from_file :
    string ->
    (t, [> Otfm.error | `Read_error of string]) result
end

(** [cut ?col ?size font text] returns an image displaying [text] with
   color [col] and size [size] using font [font], as well as a
   bounding box for the text in the image. The actual bounding box is
   driver-dependent but the result of this function should provide a
   reasonable appromixation. *)
val cut :
  ?col:Color.t ->
  ?size:float ->
  Font.t ->
  string ->
  image * box2

(** {5 Low-level functions} *)
val glyphs_of_string : Font.t -> string -> glyph list
val layout : Font.t -> font_size:float -> string -> int list * v2 list * float
val text_length : Font.t -> font_size:float -> string -> float
val bbox : size:float -> Font.t -> string -> box2
