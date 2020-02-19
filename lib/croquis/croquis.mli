open Gg

type point = float * float

module Scaling : sig
  type 'a t

  val id : float t
  val linear :
    domain:(float * float) ->
    range:(float * float) ->
    float t
end

module Viewport : sig
  type t

  val id : t

  val make :
    ?scale_x:float Scaling.t ->
    ?scale_y:float Scaling.t ->
    unit ->
    t

  val linear :
    xlim:float * float ->
    ylim:float * float ->
    size:float * float ->
    t

  val scale_x : t -> float -> float
  val scale_y : t -> float -> float
  val scale : t -> point -> point
end

module Font : sig
  type t

  val ascender : t -> float
  val descender : t -> float
  val xmin : t -> float
  val ymin : t -> float
  val xmax : t -> float
  val ymax : t -> float
  val default : t
  val free_sans : t
  val free_sans_bold : t
end

type thickness = [
  | `normal
  | `thick
]

type point_shape = [
  | `bullet
  | `circle
]

module Picture : sig
  type t

  val void : t

  val points :
    ?vp:Viewport.t ->
    ?col:Color.t ->
    ?shape:point_shape ->
    x:float array ->
    y:float array ->
    unit ->
    t

  val rect :
    ?vp:Viewport.t ->
    ?draw:Color.t ->
    ?fill:Color.t ->
    ?thickness:thickness ->
    xmin:float ->
    xmax:float ->
    ymin:float ->
    ymax:float ->
    unit ->
    t

  val path :
    ?vp:Viewport.t ->
    ?col:Color.t ->
    ?thickness:thickness ->
    ?arrow_head:bool ->
    point list ->
    t

  val blend : t list -> t
  val blend2 : t -> t -> t

  val translate :
    ?dx:float ->
    ?dy:float ->
    t -> t

  val scale :
    ?sx:float ->
    ?sy:float ->
    t -> t

  val reshape :
    t ->
    bbox:box2 ->
    t

  val crop : t -> box2 -> t

  val pileup : t list -> t

  val vstack :
    ?align:[`none | `centered | `left | `right ] ->
    t list ->
    t

  val hstack :
    ?align:[`none | `centered | `top | `bottom ] ->
    t list ->
    t
  
  val text :
    ?vp:Viewport.t ->
    ?col:Color.t ->
    ?size:float ->
    ?font:Font.t ->
    x:float ->
    y:float ->
    string ->
    t
end

module Plot : sig
  type t

  val points :
    ?title:string ->
    ?col:Color.t ->
    ?shape:[`bullet | `circle] ->
    float array ->
    float array ->
    t

  val render :
    ?width:float ->
    ?height:float ->
    t list ->
    Picture.t
end

module Layout : sig
  type t

  val simple : Picture.t -> t

  val render_pdf :
    ?width:float ->
    ?height:float ->
    t ->
    string ->
    unit
end
