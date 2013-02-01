open Batteries
open Printf
open Biocaml

module Location = struct
  type t = string * Range.t

  let make chr st ed =
    if st > ed then (
      printf "Location.make: incorrect coordinates %d %d\n" st ed ;
      assert false
    ) ;
    (chr, Range.make st ed)

  let chr = fst

  let st (_,{Range.lo}) = lo

  let ed (_,{Range.hi}) = hi

  let range_overlaps s s' = Range.(
    let p s s' = (s.hi >= s'.lo) && (s.hi <= s'.hi) in 
    p s s' || p s' s
  )
  let overlaps (chr1,r1) (chr2,r2) = 
    chr1 = chr2 && (range_overlaps r1 r2)

  let range_dist r r' = Range.(
    if range_overlaps r r' then 0
    else
      let a, b = r.hi - r'.lo, r.lo - r'.hi in
      min (abs a) (abs b)
  )
  let dist (chr1,r1) (chr2,r2) = 
    if chr1 <> chr2 then raise (Invalid_argument "MBSchema.Location.dist") ;
    range_dist r1 r2


  let range_pos ~from r = Range.(
    if range_overlaps from r then 0
    else
      let a, b = r.hi - from.lo, r.lo - from.hi in
      if abs a < abs b then a else b
  )
  let position ~from (chr2,r2) = 
    let (chr1,r1) = from in
    if chr1 <> chr2 then raise (Invalid_argument "MBSchema.Location.position") ;
    range_pos ~from:r1 r2

  let range_stranded_pos ~from ~strand r = 
    let p = range_pos ~from r in
    if strand = `Sense then p else (- p)
  let stranded_position ~from ~strand (chr,r) =
    if fst from <> chr then raise (Invalid_argument "MBSchema.Location.stranded_position") ;
    range_stranded_pos ~from:(snd from) ~strand r

  let center (chr, { Range.lo ; hi }) =
    let c = (lo + hi) / 2 in
    chr,
    Range.make c (if (lo + hi) mod 2 = 0 then c else c + 1)

  let relmove a b (chr, { Range.lo ; hi }) = 
    (chr, Range.make (lo + a) (hi + b))


  let upstream ~up ~down strand (chr, { Range.lo ; hi }) =
  match strand with
    | `Sense ->
	make chr (max 0 (lo - up)) (max 0 (lo + down))
    | `Antisense ->
	make chr (max 0 (hi - down)) (max 0 (hi + up))

let size (_,r) = Range.size r

let to_string (chr, { Range.lo ; hi }) = 
  sprintf "%s:%d-%d" chr lo hi

let of_string s =
  try Scanf.sscanf s "%s@:%d-%d" make
  with Scanf.Scan_failure _ -> (
    Core.Std.failwithf "MBSchema.Location.of_string: invalid format %s" s ()
  )
end

module Transcript = struct
  type t = {
    id : string ;
    gene_id : string ;
    strand : [`Sense | `Antisense] ;
    exons : Location.t list
  }

  let tss transcript =
    let open Location in 
    let exon1 = List.hd transcript.exons in
    let pos = match transcript.strand with
    | `Sense -> st exon1
    | `Antisense -> ed exon1 in 
    make (chr exon1) pos pos

  let position2tss transcript loc = 
    Location.stranded_position
      ~from:(tss transcript)
      ~strand:transcript.strand
      loc
end

module Gene = struct
  type t = {
    id : string ;
    aliases : (string * string) list ;
    transcripts : Transcript.t list
  }

  let symbol g =
    try Some (List.assoc "symbol" g.aliases)
    with Not_found -> None
end


module ConfigFile = struct
  open Sexplib.Std

  type t = statement list
  and statement = 
    | Condition of condition
    | Sample of sample
    | Model of model
  and condition = string
  and sample = {
    sample_id : string ;
    sample_type : sample_type ;
    sample_files : string list ;
    sample_model : string ;
    sample_condition : string ;
  }
  and sample_type =
    | ChIP_seq_input
    | TF_ChIP_seq of string
    | RNA_seq
  and model = {
    model_id : string ;
    model_genome : genome ;
  }
  and genome = [`mm9]
  with sexp

  let load path =
    Sexplib.Sexp.load_sexp_conv_exn path t_of_sexp

  let save cfg path =
    Sexplib.Sexp.save_hum path (sexp_of_t cfg)
end














