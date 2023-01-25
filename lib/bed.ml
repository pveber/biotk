open Core
open Printf

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

module type Base = sig
  type t
  val loc : t -> GLoc.t
  val from_fields : string list -> t
  val to_fields : t -> string list
end

module type Item = sig
  type t
  val loc : t -> GLoc.t
  val of_line : Line.t -> t
  val to_line : t -> string
end

module type S = sig
  type item
  val load : string -> item list
  val load_as_lmap : string -> item GAnnot.LMap.t
  val save : item list -> string -> unit
end

module Make(T : Base) = struct
  module Item = struct
    type t = T.t
    let loc = T.loc
    let of_line line =
      line
      |> Line.split ~on:'\t'
      |> T.from_fields
    let to_line r =
      T.to_fields r
      |> String.concat ~sep:"\t"
  end

  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l -> Item.of_line (Line.of_string_unsafe l))

  let save bed fn =
    Out_channel.with_file fn ~f:(fun oc ->
        List.iter bed ~f:(fun item ->
            Out_channel.output_string oc (Item.to_line item) ;
            Out_channel.output_char oc '\n'
          )
      )

  let load_as_lmap fn = (* FIXME: could use stream to read bed file *)
    load fn
    |> Stdlib.List.to_seq
    |> Seq.map (fun x -> T.loc x, x)
    |> GAnnot.LMap.of_seq
end

type fields = string list
[@@deriving show]

module Bed3 = struct
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
  }
  module Base = struct
    type t = item

    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd
    ]
  end
  include Make(Base)
  module Item = struct
    include Item
    let of_loc l = {
      chrom = l.GLoc.chr ;
      chromStart = l.lo ;
      chromEnd = l.hi ;
    }
  end

end

module Bed4 = struct
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
  }
  module Base = struct
    type t = item
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ; r.name
    ]
  end
  include Make(Base)
end

module Bed5 = struct
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
  }
  module Base = struct
    type t = item
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ; score = Int.of_string score }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%d" r.score
    ]
  end
  include Make(Base)

  module Item = struct
    include Item
    let to_bed4 it =
      { Bed4.chrom = it.chrom ; chromStart = it.chromStart ;
        chromEnd = it.chromEnd ; name = it.name }
  end
end

module Bed6 = struct
  type item = {
    chrom : string ;
    chromStart : int ;
    chromEnd : int ;
    name : string ;
    score : int ;
    strand : strand ;
  }
  module Base = struct
    type t = item
    let loc r = GLoc.{ chr = r.chrom ; lo = r.chromStart ; hi = r.chromEnd }

    let from_fields = function
      | chrom :: chromStart :: chromEnd :: name :: score :: strand :: _ ->
        { chrom ;
          chromStart = Int.of_string chromStart ;
          chromEnd = Int.of_string chromEnd ;
          name ;
          score = Int.of_string score ;
          strand = (
            match parse_strand strand with
            | Ok s -> s
            | Error msg -> failwith msg
          ) ;
        }
      | l -> failwithf "Expected more fields, got %s" (show_fields l) ()

    let to_fields r = [
      r.chrom ; sprintf "%d" r.chromStart ; sprintf "%d" r.chromEnd ;
      r.name ; sprintf "%d" r.score ;
      (match r.strand with
       | `Not_relevant -> "."
       | `Unknown -> "?"
       | `Plus -> "+"
       | `Minus -> "-" )
    ]
  end
  include Make(Base)
end

module Base = struct
  type t = GLoc.t * fields

  let loc = fst
  let from_fields xs = Bed3.Base.(from_fields xs |> loc), xs
  let to_fields = snd
end
include Make(Base)
