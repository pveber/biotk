open Core
open Biocaml_base
open Printf

type 'a item = [
  | `Comment of string
  | `Record of 'a
]

type strand = [
  | `Plus
  | `Minus
  | `Not_relevant
  | `Unknown
]

let parse_strand = function
  | "." -> Ok `Not_relevant
  | "?" -> Ok `Unknown
  | "+" -> Ok `Plus
  | "-" -> Ok `Minus
  | s -> Error s


let unparse_strand = function
  | `Not_relevant -> "."
  | `Unknown -> "?"
  | `Plus -> "+"
  | `Minus -> "-"

let parse_item f line =
  match (line : Line.t :> string) with
  | "" -> `Comment ""
  | line ->
    if Char.(line.[0] = '#')
    then `Comment (String.slice line 1 0)
    else
      let fields = String.split ~on:'\t' line in
      `Record (f fields)

let unparse_item f = function
  | `Comment c -> sprintf "#%s\n" c
  | `Record r ->
      sprintf "%s\n"
        (String.concat ~sep:"\t" (f r))

module type Base = sig
  type t
  val loc : t -> GLoc.t
  val from_fields : string list -> t
  val to_fields : t -> string list
end

module type S = sig
  type t
  val loc : t -> GLoc.t
  val of_line : Line.t -> t item
  val to_line : t item -> string
  val load : string -> t item list
  val load_as_lmap : string -> t GAnnot.LMap.t
end

module Make(T : Base) = struct
  let of_line = parse_item T.from_fields
  let to_line = unparse_item T.to_fields

  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l -> of_line (Line.of_string_unsafe l))

  let load_as_lmap fn = (* FIXME: could use stream to read bed file *)
    load fn
    |> Stream.of_list
    |> CFStream.Stream.filter_map ~f:(function
        | `Comment _ -> None
        | `Record x -> Some (T.loc x, x)
      )
    |> GAnnot.LMap.of_stream

end

module Bed3 = struct
  module Base = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
    }

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
        }
      | _ -> assert false

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd
    ]
  end
  include Base
  include Make(Base)
end

module Bed4 = struct
  module Base = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
    }

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name }
      | _ -> assert false

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ; r.name
    ]
  end
  include Base
  include Make(Base)
end

module Bed5 = struct
  module Base = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : float ;
    }

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ; score = Float.of_string score }
      | _ -> assert false

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%g" r.score
    ]
  end
  include Base
  include Make(Base)

  let to_bed4 = function
    | `Comment c -> `Comment c
    | `Record r ->
      `Record { Bed4.chrom = r.chrom ; chromStart = r.chromStart ;
                chromEnd = r.chromEnd ; name = r.name }
end

module Bed6 = struct
  module Base = struct
    type t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      name : string ;
      score : float ;
      strand : strand ;
    }

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: strand :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ;
          score = Float.of_string score ;
          strand = (
            match parse_strand strand with
            | Ok s -> s
            | Error msg -> failwith msg
          ) ;
        }
      | _ -> assert false

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%g" r.score ;
      (match r.strand with
       | `Not_relevant -> "."
       | `Unknown -> "?"
       | `Plus -> "+"
       | `Minus -> "-" )
    ]
  end
  include Base
  include Make(Base)
end
