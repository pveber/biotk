type genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `mm10 | `sacCer2 ]
val string_of_genome : [< genome] -> string

module CustomTrack : sig
  type option = [
  | `track_type of string
  | `name of string
  | `description of string
  | `visibility of [ `hide | `full | `dense ]
  | `color of int * int * int
  | `altColor of int * int * int
  | `priority of int
  | `autoScale of bool
  | `gridDefault of bool
  | `maxHeightPixels of int * int * int
  | `graphType of [ `bar | `points ]
  | `viewLimits of [ `lower | `upper ]
  | `yLineMark of float
  | `yLineOnOff of bool
  | `windowingFunction of [ `maximum | `mean | `minimum ]
  | `smoothingWindow of [ `off | `width of int ]
  | `dataUrl of string
  | `bigDataUrl of string
  | `useScore of bool
  ]

  val url : [< genome] -> option list -> string
end
