type genome = [ `dm3 | `hg18 | `hg19 | `mm8 | `mm9 | `mm10 | `sacCer2 ]

let string_of_genome = function
| `dm3 -> "dm3"
| `hg18 -> "hg18"
| `hg19 -> "hg19"
| `mm8 -> "mm8"
| `mm9 -> "mm9"
| `mm10 -> "mm10"
| `sacCer2 -> "sacCer2"

module CustomTrack = struct
  open Printf

  type option = [
    `track_type of string
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

  let unparse_option buf = function
      `name n -> bprintf buf " name=\"%s\"" n
    | `track_type t -> bprintf buf " type=%s" t
    | `dataUrl u -> bprintf buf " dataUrl=%s" u
    | `bigDataUrl u -> bprintf buf " bigDataUrl=%s" u
    | `description d -> bprintf buf " description=\"%s\"" d
    | `visibility v -> bprintf buf " visibility=%s"
	(match v with
	     `full -> "full"
	   | `dense -> "dense"
	   | `hide -> "hide")
    | `color (r,g,b) ->
	bprintf buf " color=%d,%d,%d" r g b
    | `altColor (r,g,b) ->
	bprintf buf " altColor=%d,%d,%d" r g b
    | `yLineOnOff b ->
	bprintf buf " yLineOnOff=%s" (if b then "on" else "off")
    | `priority p ->
	bprintf buf " priority=%d" p
    | `useScore b -> bprintf buf " useScore=%d" (if b then 1 else 0)
    | _ -> assert false

  let header_of_options opt =
    let buf = Buffer.create 1024 in
      bprintf buf "track" ;
      List.iter (unparse_option buf) opt ;
      Buffer.contents buf

  let url org track_options =
    let base = "http://genome.ucsc.edu/cgi-bin/hgTracks?db=" ^ (string_of_genome org)
    and escaped_custom_text = Netencoding.Url.encode ~plus:false (header_of_options track_options)
    in
      sprintf "%s&hgct_customText=%s" base escaped_custom_text
end
