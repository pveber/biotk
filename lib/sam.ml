open Core
open Prelude

module Seq = Stdlib.Seq
open Result.Monad_infix

let ( >>?~ )
    (x : ('a option, 'e) result)
    (f : 'a -> ('b, 'e) result)
    : ('b option, 'e) result
    =
  let open Result.Monad_infix in
  x >>= function
  | None -> Ok None
  | Some x -> f x >>| Option.some

(******************************************************************************)
(* Header Types                                                               *)
(******************************************************************************)
type header_item_tag = [
| `HD | `SQ | `RG | `PG | `CO
| `Other of string
] [@@deriving sexp, show]

type tag_value = string * string
[@@deriving sexp]

type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
[@@deriving sexp]

type group_order = [ `None | `Query | `Reference ]
[@@deriving sexp]

type header_line = {
  version : string;
  sort_order : sort_order option;
  group_order: group_order option;
} [@@deriving sexp]

type ref_seq = {
  name : string;
  length : int;
  assembly : string option;
  md5 : string option;
  species : string option;
  uri : string option;
} [@@deriving sexp]

type platform = [
| `Capillary | `LS454 | `Illumina | `Solid
| `Helicos | `Ion_Torrent | `Pac_Bio
] [@@deriving sexp]

type read_group = {
  id : string;
  seq_center : string option;
  description : string option;
  run_date : [`Date of string | `Time of string] option;
  flow_order : string option;
  key_seq : string option;
  library : string option;
  program : string option;
  predicted_median_insert_size : int option;
  platform : platform option;
  platform_unit : string option;
  sample : string option;
} [@@deriving sexp]

type program = {
  id : string;
  name : string option;
  command_line : string option;
  previous_id : string option;
  description : string option;
  version : string option;
} [@@deriving sexp]

type header_item = [
| `HD of header_line
| `SQ of ref_seq
| `RG of read_group
| `PG of program
| `CO of string
| `Other of string * tag_value list
] [@@deriving sexp]

type header = {
  version : string option;
  sort_order : sort_order option;
  group_order : group_order option;
  ref_seqs : ref_seq list;
  read_groups : read_group list;
  programs : program list;
  comments : string list;
  others : (string * tag_value list) list;
}

type error = [
  | `SAM_parsing_error of string * int option
]

let empty_header = {
  version = None;
  sort_order = None;
  group_order = None;
  ref_seqs = [];
  read_groups = [];
  programs = [];
  comments = [];
  others = [];
}


(******************************************************************************)
(* Alignment Types                                                            *)
(******************************************************************************)
module Flags = struct
  type t = int
  [@@deriving sexp]

  let of_int x =
    if (0 <= x) && (x <= 65535) then
      Ok x
    else
      Error `Flag_out_of_range

  let flag_is_set s f = (f land s) <> 0

  let has_multiple_segments            = flag_is_set 0x1
  let each_segment_properly_aligned    = flag_is_set 0x2
  let segment_unmapped                 = flag_is_set 0x4
  let next_segment_unmapped            = flag_is_set 0x8
  let seq_is_reverse_complemented      = flag_is_set 0x10
  let next_seq_is_reverse_complemented = flag_is_set 0x20
  let first_segment                    = flag_is_set 0x40
  let last_segment                     = flag_is_set 0x80
  let secondary_alignment              = flag_is_set 0x100
  let not_passing_quality_controls     = flag_is_set 0x200
  let pcr_or_optical_duplicate         = flag_is_set 0x400
  let supplementary_alignment          = flag_is_set 0x800
end

type cigar_op = [
  | `Alignment_match of int
  | `Insertion of int
  | `Deletion of int
  | `Skipped of int
  | `Soft_clipping of int
  | `Hard_clipping of int
  | `Padding of int
  | `Seq_match of int
  | `Seq_mismatch of int
] [@@deriving sexp]

type optional_field_value = [
| `A of char
| `i of Int64.t
| `f of float
| `Z of string
| `H of string
| `B of char * string list
] [@@deriving sexp]

type optional_field = {
  tag : string;
  value : optional_field_value
} [@@deriving sexp]

type rnext = [`Value of string | `Equal_to_RNAME]
[@@deriving sexp]

type alignment = {
  qname : string option;
  flags : Flags.t;
  rname : string option;
  pos : int option;
  mapq : int option;
  cigar : cigar_op list;
  rnext : rnext option;
  pnext : int option;
  tlen : int option;
  seq: string option;
  qual: Phred_score.t list;
  optional_fields : optional_field list;
} [@@deriving sexp]



(******************************************************************************)
(* Header Parsers and Constructors                                            *)
(******************************************************************************)
let parse_header_version s =
  let err () = Error.msgf "invalid version %s" s in
  match String.lsplit2 ~on:'.' s with
  | None -> err ()
  | Some (a,b) ->
    if (String.for_all a ~f:Char.is_digit)
      && (String.for_all b ~f:Char.is_digit)
    then
      Ok (Int.of_string a, Int.of_string b)
    else
      err ()

let header_line ~version ?sort_order ?group_order () =
  parse_header_version version >>| fun _ ->
  {version; sort_order; group_order}

let ref_seq
    ~name ~length
    ?assembly ?md5 ?species ?uri
    ()
    =
  let is_name_first_char_ok = function
    | '!' .. ')' | '+' .. '<' | '>' .. '~' -> true
    | _ -> false
  in

  let is_name_other_char_ok = function '!' .. '~' -> true | _ -> false in

  (if (1 <= length) && (length <= 2147483647) then
      Ok length
   else
      Error.msgf "invalid reference sequence length %d" length
  ) >>= fun length ->

  (if (String.length name > 0)
      && (String.foldi name ~init:true ~f:(fun i accum c ->
        accum && (
          if i = 0 then is_name_first_char_ok c
          else is_name_other_char_ok c
        ) ) )
   then
      Ok name
   else
      Error.msgf "invalid reference sequence name (%s)" name
  ) >>= fun name ->

  Ok {name; length; assembly; md5; species; uri}


let read_group
    ~id ?seq_center ?description ?run_date ?flow_order
    ?key_seq ?library ?program ?predicted_median_insert_size
    ?platform ?platform_unit ?sample
    ()
    =
  (match run_date with
  | None -> Ok None
  | Some run_date ->
    try Ok (Some (`Date run_date))
    with _ ->
      try Ok (Some (`Time run_date))
      with _ ->
        Error.msgf "Invalid run date or time (%s)" run_date
  ) >>= fun run_date ->

  (match flow_order with
  | None -> Ok None
  | Some "" -> Error.msg "invalid empty flow order"
  | Some "*" -> Ok flow_order
  | Some x ->
    if String.for_all x ~f:(function
    | 'A' | 'C' | 'M' | 'G' | 'R' | 'S' | 'V' | 'T' | 'W'| 'Y' | 'H'
    | 'K' | 'D' | 'B' | 'N' -> true
    | _ -> false
    )
    then
      Ok flow_order
    else
      Error.msgf "invalid flow order (%s)" x
  ) >>| fun flow_order ->

  {
    id; seq_center; description; run_date; flow_order; key_seq;
    library; program; predicted_median_insert_size;
    platform; platform_unit; sample;
  }

let header
    ?version ?sort_order ?group_order ?(ref_seqs=[]) ?(read_groups=[])
    ?(programs=[]) ?(comments=[]) ?(others=[])
    ()
  =
  let open Let_syntax.Result in
  let* () =
    match version with
    | None -> Ok ()
    | Some x -> parse_header_version x >>| ignore
  in
  let* () =
    if Option.is_some sort_order && Poly.(version = None) then
      Error.msg "sort order cannot be defined without version"
    else Ok ()
  in
  let* () =
    let maybe_dup =
      List.map ref_seqs ~f:(fun (x:ref_seq) -> x.name)
      |> List.find_a_dup ~compare:String.compare
    in
    match maybe_dup with
    | None -> Ok ()
    | Some dup ->
      Error.msgf "duplicate reference sequence name (%s)" dup
  in
  Ok {
    version; sort_order; group_order; ref_seqs; read_groups;
    programs; comments; others;
  }


let parse_header_item_tag s =
  let is_letter = function 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false in
  match String.chop_prefix s ~prefix:"@" with
  | None -> Error.msg "header item tag must begin with @"
  | Some "HD" -> Ok `HD
  | Some "SQ" -> Ok `SQ
  | Some "RG" -> Ok `RG
  | Some "PG" -> Ok `PG
  | Some "CO" -> Ok `CO
  | Some x ->
    if (String.length x = 2)
      && (String.for_all x ~f:is_letter)
    then
      Ok (`Other x)
    else
      Error.msgf "invalid header item tag (%s)" s

let parse_tag_value s =
  let parse_tag s =
    if (String.length s = 2)
      && (match s.[0] with 'A' .. 'Z' | 'a' .. 'z' -> true | _ -> false)
      && (match s.[1] with
          | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
          | _ -> false
      )
    then
      Ok s
    else
      Error.msgf "invalid tag %s" s
  in
  let parse_value tag s =
    if String.(s <> "")
      && (String.for_all s ~f:(function ' ' .. '~' -> true | _ -> false))
    then
      Ok s
    else
      Error.msgf "tag %s has invalid value %s" tag s
  in
  match String.lsplit2 s ~on:':' with
  | None ->
    Error.msgf "tag-value %s not colon separated" s
  | Some (tag,value) ->
    parse_tag tag >>= fun tag ->
    parse_value tag value >>= fun value ->
    Ok (tag, value)

(** Find all occurrences of [x'] in the association list [l]. *)
let find_all l x' =
  let rec loop accum = function
    | [] -> accum
    | (x,y)::l ->
      let accum = if Poly.(x = x') then y::accum else accum in
      loop accum l
  in
  List.rev (loop [] l)

(** Find exactly 1 occurrence [x] in association list [l]. Return
    error if [x] is not defined exactly once. *)
let find1 header_item_tag l x =
  match find_all l x with
  | [] ->
    Error.msgf "required tag %s not found in %s header item" x (show_header_item_tag header_item_tag)
  | y :: [] -> Ok y
  | _ ->
    Error.msgf "tag_found_multiple_times (%s)" (show_header_item_tag header_item_tag)

(** Find 0 or 1 occurrence [x] in association list [l]. Return
    error if [x] is defined more than once. *)
let find01 header_item_tag l x =
  match find_all l x with
  | [] -> Ok None
  | y :: [] -> Ok (Some y)
  | _  ->
    Error.msgf "tag found multiple times (%s)" (show_header_item_tag header_item_tag)

(** Assert that [tvl] contains at most the given [tags]. *)
let assert_tags header_item_tag tvl tags =
  let expected_tags = String.Set.of_list tags in
  let got_tags = List.map tvl ~f:fst |> String.Set.of_list in
  let unexpected_tags = Set.diff got_tags expected_tags in
  if Set.length unexpected_tags = 0 then
    Ok ()
  else
    Error.msgf "unexpected tag for given header item type (%s)" (show_header_item_tag header_item_tag)

let parse_sort_order = function
  | "unknown" -> Ok `Unknown
  | "unsorted" -> Ok `Unsorted
  | "queryname" -> Ok `Query_name
  | "coordinate" -> Ok `Coordinate
  | x -> Error.msgf "invalid sort order (%s)" x

let parse_group_order = function
  | "none" -> Ok `None
  | "query" -> Ok `Query
  | "reference" -> Ok `Reference
  | x -> Error.msgf "invalid group order (%s)" x

let parse_header_line tvl =
  find1 `HD tvl "VN" >>= fun version ->
  find01 `HD tvl "SO" >>?~
  parse_sort_order >>= fun sort_order ->
  find01 `HD tvl "GO" >>?~
  parse_group_order >>= fun group_order ->
  assert_tags `HD tvl ["VN"; "SO"; "GO"] >>= fun () ->
  header_line ~version ?sort_order ?group_order ()

let parse_ref_seq tvl =
  find1 `SQ tvl "SN" >>= fun name ->
  find1 `SQ tvl "LN" >>= fun length ->
  (try Ok (Int.of_string length)
   with _ ->
     Error.msgf "invalid reference sequence length (%s)" length
  ) >>= fun length ->
  find01 `SQ tvl "AS" >>= fun assembly ->
  find01 `SQ tvl "M5" >>= fun md5 ->
  find01 `SQ tvl "SP" >>= fun species ->
  find01 `SQ tvl "UR" >>= fun uri ->
  assert_tags `SQ tvl ["SN";"LN";"AS";"M5";"SP";"UR"] >>= fun () ->
  ref_seq ~name ~length ?assembly ?md5 ?species ?uri ()

let parse_platform = function
  | "CAPILLARY" -> Ok `Capillary
  | "LS454" -> Ok `LS454
  | "ILLUMINA" -> Ok `Illumina
  | "SOLID" -> Ok `Solid
  | "HELICOS" -> Ok `Helicos
  | "IONTORRENT" -> Ok `Ion_Torrent
  | "PACBIO" -> Ok `Pac_Bio
  | x -> Error.msgf "unknown platform %s" x

let parse_read_group tvl =
  find1 `RG tvl "ID" >>= fun id ->
  find01 `RG tvl "CN" >>= fun seq_center ->
  find01 `RG tvl "DS" >>= fun description ->
  find01 `RG tvl "DT" >>= fun run_date ->
  find01 `RG tvl "FO" >>= fun flow_order ->
  find01 `RG tvl "KS" >>= fun key_seq ->
  find01 `RG tvl "LB" >>= fun library ->
  find01 `RG tvl "PG" >>= fun program ->
  find01 `RG tvl "PI" >>?~ (fun predicted_median_insert_size ->
  try Ok (Int.of_string predicted_median_insert_size)
  with _ ->
    Error.msgf
      "invalid predicted median insert size (%s)"
      predicted_median_insert_size
  ) >>= fun predicted_median_insert_size ->
  find01 `RG tvl "PL" >>?~
  parse_platform >>= fun platform ->
  find01 `RG tvl "PU" >>= fun platform_unit ->
  find01 `RG tvl "SM" >>= fun sample ->
  assert_tags `RG tvl
    ["ID";"CN";"DS";"DT";"FO";"KS";"LB";"PG";"PI";"PL";"PU";"SM"]
  >>= fun () ->
  read_group
    ~id ?seq_center ?description ?run_date ?flow_order ?key_seq
    ?library ?program ?predicted_median_insert_size
    ?platform ?platform_unit ?sample ()

let parse_program tvl =
  find1 `PG tvl "ID" >>= fun id ->
  find01 `PG tvl "PN" >>= fun name ->
  find01 `PG tvl "CL" >>= fun command_line ->
  find01 `PG tvl "PP" >>= fun previous_id ->
  find01 `PG tvl "DS" >>= fun description ->
  find01 `PG tvl "VN" >>= fun version ->
  assert_tags `PG tvl ["ID";"PN";"CL";"PP";"DS";"VN"] >>| fun () ->
  {id; name; command_line; previous_id; description; version}

let parse_header_item line =
  let parse_data tag tvl = match tag with
    | `HD -> parse_header_line tvl >>| fun x -> `HD x
    | `SQ -> parse_ref_seq tvl >>| fun x -> `SQ x
    | `RG -> parse_read_group tvl >>| fun x -> `RG x
    | `PG -> parse_program tvl >>| fun x -> `PG x
    | `Other tag -> Ok (`Other (tag,tvl))
    | `CO -> assert false
  in
  match String.lsplit2 ~on:'\t' line with
  | None ->
    Error.msg "header line contains no tabs"
  | Some (tag, data) ->
    parse_header_item_tag tag >>= function
    | `CO -> Ok (`CO data)
    | tag ->
      match String.split ~on:'\t' data with
      | [] -> assert false
      | ""::[] ->
        Error.msg "header line contains no data"
      | tvl ->
        List.map tvl ~f:parse_tag_value |> Result.all >>= fun tvl ->
        parse_data tag tvl

let map_sam_parsing_error lno =
  Result.map_error ~f:(fun (`Msg msg) ->
      `SAM_parsing_error (msg, Some lno)
    )

let parse_header_gen src =
  let open Let_syntax.Result in
  let return ({version; sort_order; group_order; _} as x) lno maybe_al =
    let ref_seqs = List.rev x.ref_seqs in
    let read_groups = List.rev x.read_groups in
    let programs = List.rev x.programs in
    let comments = List.rev x.comments in
    let others = List.rev x.others in
    let+ header =
      header
        ?version ?sort_order ?group_order ~ref_seqs ~read_groups
        ~programs ~comments ~others ()
      |> Result.map_error ~f:(fun (`Msg msg) ->
          `SAM_parsing_error (msg, None)
        )
    in
    header, lno, maybe_al
  in
  let rec loop lno hdr =
    match src () with
    | None -> return hdr lno None
    | Some line ->
      let lno = lno + 1 in
      if String.length line = 0 then
        Error (`SAM_parsing_error ("empty line", Some lno))
      else if Char.(line.[0] <> '@') then
        return hdr (lno + 1) (Some line)
      else (
        let* header_item =
          parse_header_item line
          |> map_sam_parsing_error lno
        in
        match header_item with
        | `HD ({version; sort_order; group_order} : header_line) -> (
            match hdr.version with
            | Some _ ->
              Error (`SAM_parsing_error ("multiple @HD lines not allowed", Some lno))
            | None ->
              loop
                lno
                {hdr with version = Some version; sort_order; group_order}
          )
        | `SQ x -> loop lno {hdr with ref_seqs = x::hdr.ref_seqs}
        | `RG x -> loop lno {hdr with read_groups = x::hdr.read_groups}
        | `PG x -> loop lno {hdr with programs = x::hdr.programs}
        | `CO x -> loop lno {hdr with comments = x::hdr.comments}
        | `Other x -> loop lno {hdr with others = x::hdr.others}
      )
  in
  loop 0 empty_header

let list_iterator xs =
  let cursor = ref xs in
  fun () ->
    match !cursor with
    | [] -> None
    | h :: t -> cursor := t ; Some h

let parse_header buf =
  let open Let_syntax.Result in
  let lines = String.split_lines buf in
  let src = list_iterator lines in
  let+ header, _, _ = parse_header_gen src in
  header


(******************************************************************************)
(* Alignment Parsers and Constructors                                         *)
(******************************************************************************)
let alignment
    ?ref_seqs ?qname ~flags ?rname ?pos ?mapq ?(cigar=[])
    ?rnext ?pnext ?tlen ?seq ?(qual=[])
    ?(optional_fields=[])
    ()
  =
  let open Let_syntax.Result in
  let* () =
    match ref_seqs, rname with
    | None, _ | _, None -> Ok ()
    | Some ref_seqs, Some rname ->
      if Set.mem ref_seqs rname then Ok ()
      else
        Error.msgf "RNAME not defined in any SQ line (%s)" rname
  in
  let* () =
    match ref_seqs, rnext with
    | None, _ | _, None -> Ok ()
    | Some _, Some `Equal_to_RNAME ->
      Ok () (* error will already be detected in RNAME check above *)
    | Some ref_seqs, Some (`Value rnext) ->
      if Set.mem ref_seqs rnext then Ok ()
      else
        Error.msgf "RNEXT not defined in any SQ line (%s)" rnext
  in
  let* () =
    match seq, qual with
    | _, [] -> Ok ()
    | None, _ -> Error.msg "QUAL provided without SEQ"
    | Some seq, _ ->
      let s = String.length seq in
      let q = List.length qual in
      if s = q then Ok ()
      else
        Error.msgf "SEQ and QUAL lengths differ (%d, %d)" s q
  in
  let* () =
    let maybe_dup =
      List.map optional_fields ~f:(fun x -> x.tag)
      |> List.find_a_dup ~compare:String.compare
    in
    match maybe_dup with
    | None -> Ok ()
    | Some dup ->
      Error.msgf "TAG occurs more than once (%s)" dup
  in
  Ok {
    qname; flags; rname; pos; mapq; cigar;
    rnext; pnext; tlen; seq; qual; optional_fields
  }

let parse_int_range field lo hi s =
  try
    let n = Int.of_string s in
    if (lo <= n) && (n <= hi) then
      Ok n
    else
      Error.msgf "integer for field %s out of range (%d not in [%d, %d])" field n lo hi
  with _ ->
    Error.msgf "value for field %s not an integer (%s)" field s

(** Parse a string that can either by "*" or some other regexp, with
    "*" denoting [None]. The given regexp [re] should include "*" as
    one of the alternatives. *)
let parse_opt_string field re s =
  if not (Re.execp re s) then
    Error.msgf "invalid value for field %s (%s)" field s
  else
    match s with
    | "*" -> Ok None
    | _ -> Ok (Some s)

let qname_re =
  let open Re in
  alt [
    char '*';
    repn (alt [rg '!' '?'; rg 'A' '~']) 1 (Some 255);
  ]
  |> compile

let parse_qname s =
  parse_opt_string "QNAME" qname_re s

let parse_flags s =
  match (
    try Flags.of_int (Int.of_string s)
    with _ -> Error `Invalid_int
  )
  with
  | Ok _ as r -> r
  | Error _ -> Error.msgf "invalid FLAG (%s)" s

let rname_re = Re.Perl.compile_pat "^\\*|[!-()+-<>-~][!-~]*$"
let parse_rname s =
  parse_opt_string "RNAME" rname_re s

let parse_pos s =
  parse_int_range "POS" 0 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let parse_mapq s =
  parse_int_range "MAPQ" 0 255 s >>| function
  | 255 -> None
  | x -> Some x

let positive i =
  if i > 0 then Ok i
  else Error.msg "positive argument expected for cigar operation"

let cigar_op_alignment_match i = Result.(positive i >>| fun i -> `Alignment_match i)
let cigar_op_insertion i = Result.(positive i >>| fun i -> `Insertion i)
let cigar_op_deletion i = Result.(positive i >>| fun i -> `Deletion i)
let cigar_op_skipped i = Result.(positive i >>| fun i -> `Skipped i)
let cigar_op_soft_clipping i = Result.(positive i >>| fun i -> `Soft_clipping i)
let cigar_op_hard_clipping i = Result.(positive i >>| fun i -> `Hard_clipping i)
let cigar_op_padding i = Result.(positive i >>| fun i -> `Padding i)
let cigar_op_seq_match i = Result.(positive i >>| fun i -> `Seq_match i)
let cigar_op_seq_mismatch i = Result.(positive i >>| fun i -> `Seq_mismatch i)

let parse_cigar text =
  let open Let_syntax.Result in
  match text with
  | "*" -> Ok []
  | "" ->
    Error.msgf "invalid cigar string (%s)" text
  | _ ->
    let ch = Scanf.Scanning.from_string text in
    let rec loop accum =
      if Scanf.Scanning.end_of_input ch then Ok accum
      else
        try
          let n = Scanf.bscanf ch "%d" Fun.id in
          let c = Scanf.bscanf ch "%c" Fun.id in
          let* x =
            match c with
            | 'M' -> cigar_op_alignment_match n
            | 'I' -> cigar_op_insertion n
            | 'D' -> cigar_op_deletion n
            | 'N' -> cigar_op_skipped n
            | 'S' -> cigar_op_soft_clipping n
            | 'H' -> cigar_op_hard_clipping n
            | 'P' -> cigar_op_padding n
            | '=' -> cigar_op_seq_match n
            | 'X' -> cigar_op_seq_mismatch n
            | other -> Error.msgf "invalid cigar operation type (%c)" other
          in
          loop (x :: accum)
        with
          _ ->
            Error.msgf "invalid cigar string (%s)" text
    in
    loop [] >>| List.rev

let rnext_re = Re.Perl.compile_pat "^\\*|=|[!-()+-<>-~][!-~]*$"
let parse_rnext s =
  if not (Re.execp rnext_re s) then
    Error.msgf "invalid RNEXT (%s)" s
  else
    match s with
    | "*" -> Ok None
    | "=" -> Ok (Some `Equal_to_RNAME)
    | _ -> Ok (Some (`Value s))

let parse_pnext s =
  parse_int_range "PNEXT" 0 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let parse_tlen s =
  parse_int_range "TLEN" ~-2147483647 2147483647 s >>| function
  | 0 -> None
  | x -> Some x

let seq_re = Re.Perl.compile_pat "^\\*|[A-Za-z=.]+$"
let parse_seq s =
  parse_opt_string "SEQ" seq_re s

let parse_qual s =
  match s with
  | "" -> Error.msg "invalid empty QUAL"
  | "*" -> Ok []
  | _ ->
    let rec loop acc i =
      if i < 0 then Ok acc
      else
        let c = s.[i] in
        match Phred_score.of_char ~offset:`Offset33 c with
        | Ok s -> loop (s :: acc) (i - 1)
        | Error _ ->
          Error.msgf "invalid character for PHRED33 (%c)" c
    in
    loop [] (String.length s - 1)


let opt_field_tag_re = Re.Perl.compile_pat "^[A-Za-z][A-Za-z0-9]$"
let opt_field_H_re = Re.Perl.compile_pat "^[0-9A-F]+$"
let opt_field_int_re = Re.Perl.compile_pat "^-?[0-9]+$"
let opt_field_float_re = Re.Perl.compile_pat "^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$"

let optional_field_value_err typ value =
  Error.msgf "invalid value (%s, %s)" typ value

let printable_character =
  let lo = Stdlib.Char.code '!' and hi = Stdlib.Char.code '~' in
  fun c ->
    let code = Stdlib.Char.code c in
    lo <= code && code <= hi

let optional_field_value_A value =
  if printable_character value then Ok (`A value)
  else optional_field_value_err "A" (sprintf "ASCII code %d" (Stdlib.Char.code value))

let optional_field_value_i i = `i i

let optional_field_value_f f = `f f

let optional_field_value_Z value =
  if String.for_all value ~f:printable_character then Ok (`Z value)
  else optional_field_value_err "Z" value

let optional_field_value_H value =
  if Re.execp opt_field_H_re value then Ok (`H value)
  else optional_field_value_err "H" value

let optional_field_value_B elt_type elts =
  let valid_args =
    match elt_type with
    | 'c' | 'C' | 's' | 'S' | 'i' | 'I' ->
      List.for_all elts ~f:(Re.execp opt_field_int_re)
    | 'f' ->
      List.for_all elts ~f:(Re.execp opt_field_float_re)
    | _ -> false
  in
  if valid_args then Ok (`B (elt_type, elts))
  else Error.msgf "invalid B value (%c, %s)" elt_type (String.concat ~sep:"," elts)

let optional_field tag value =
  if not (Re.execp opt_field_tag_re tag)
  then Error.msgf "invalid TAG (%s)" tag
  else Ok {tag; value}

let parse_optional_field_value s =
  match String.lsplit2 s ~on:':' with
  | None ->
    Error.msgf "missing TYPE in optional field (%s)" s
  | Some (typ,value) ->
    match typ with
    | "A" ->
      if String.length value = 1 then optional_field_value_A value.[0]
      else optional_field_value_err typ value
    | "i" ->
      (try
         if not (Re.execp opt_field_int_re value) then failwith "" ;
         Ok (optional_field_value_i (Int64.of_string value)) (* matching the regular expression is not enough: the number could not fit in 64 bits *)
       with _ -> optional_field_value_err typ value)
    | "f" ->
      (try
         if not (Re.execp opt_field_float_re value) then failwith "" ;
         Ok (optional_field_value_f (Float.of_string value)) (* matching the regular expression is not enough: the number could not fit in native floats *)
       with _ -> optional_field_value_err typ value)
    | "Z" -> optional_field_value_Z value
    | "H" -> optional_field_value_H value
    | "B" -> (
        match String.split ~on:',' value with
        | num_typ :: values ->
          if String.length num_typ = 1 then
            optional_field_value_B num_typ.[0] values
          else
            Error.msgf "invalid array type (%s)" num_typ
        | _ -> assert false (* [String.split] cannot return an empty list *)
      )
    | _ -> Error.msgf "invalid type (%s)" typ


let parse_optional_field s =
  match String.lsplit2 s ~on:':' with
  | None ->
    Error.msgf "missing TAG in optional field (%s)" s
  | Some (tag,s) ->
    parse_optional_field_value s >>= fun value ->
    optional_field tag value


let parse_alignment ?ref_seqs line =
  match String.split ~on:'\t' line with
  | qname :: flags :: rname :: pos :: mapq :: cigar :: rnext
     :: pnext :: tlen :: seq :: qual :: optional_fields -> (
       let open Let_syntax.Result in
       let* qname = parse_qname qname in
       let* flags = parse_flags flags in
       let* rname = parse_rname rname in
       let* pos = parse_pos pos in
       let* mapq = parse_mapq mapq in
       let* cigar = parse_cigar cigar in
       let* rnext = parse_rnext rnext in
       let* pnext = parse_pnext pnext in
       let* tlen = parse_tlen tlen in
       let* seq = parse_seq seq in
       let* qual = parse_qual qual in
       let* optional_fields =
         List.map optional_fields ~f:parse_optional_field
         |> Result.all
       in
       alignment
         ?ref_seqs ?qname ~flags ?rname ?pos ?mapq ~cigar
         ?rnext ?pnext ?tlen ?seq ~qual ~optional_fields
         ()
     )
  | _ ->
    Error.msg "alignment line contains < 12 fields"



(******************************************************************************)
(* Header Printers                                                            *)
(******************************************************************************)
let print_header_item_tag = function
  | `HD -> "@HD"
  | `SQ -> "@SQ"
  | `RG -> "@RG"
  | `PG -> "@PG"
  | `CO -> "@CO"
  | `Other x -> sprintf "@%s" x

let print_tag_value (tag,value) = sprintf "%s:%s" tag value
let print_tag_value' = sprintf "%s:%s"

let print_header_version x = print_tag_value' "VN" x

let print_sort_order x =
  print_tag_value' "SO"
    (match x with
    | `Unknown -> "unknown"
    | `Unsorted -> "unsorted"
    | `Query_name -> "queryname"
    | `Coordinate -> "coordinate"
    )

let print_group_order x =
  print_tag_value' "GO"
    (match x with
    | `None -> "none"
    | `Query -> "query"
    | `Reference -> "reference"
    )

let print_header_line ({version; sort_order; group_order} : header_line) =
  sprintf "@HD\tVN:%s%s%s"
    version
    (match sort_order with
    | None -> ""
    | Some x -> sprintf "\t%s" (print_sort_order x)
    )
    (match group_order with
    | None -> ""
    | Some x -> sprintf "\t%s" (print_group_order x)
    )

let print_ref_seq (x:ref_seq) =
  sprintf "@SQ\tSN:%s\tLN:%d%s%s%s%s"
    x.name
    x.length
    (match x.assembly with None -> "" | Some x -> sprintf "\tAS:%s" x)
    (match x.md5 with None -> "" | Some x -> sprintf "\tM5:%s" x)
    (match x.species with None -> "" | Some x -> sprintf "\tSP:%s" x)
    (match x.uri with None -> "" | Some x -> sprintf "\tUR:%s" x)

let print_platform = function
  | `Capillary -> "CAPILLARY"
  | `LS454 -> "LS454"
  | `Illumina -> "ILLUMINA"
  | `Solid -> "SOLID"
  | `Helicos -> "HELICOS"
  | `Ion_Torrent -> "IONTORRENT"
  | `Pac_Bio -> "PACBIO"

let print_read_group (x:read_group) =
  let s tag value = match value with
    |  None -> ""
    | Some x -> sprintf "\t%s:%s" tag x
  in
  sprintf "@RG\tID:%s%s%s%s%s%s%s%s%s%s%s%s"
    x.id
    (s "CN" x.seq_center)
    (s "DS" x.description)
    (s "DT" (Option.map x.run_date
               ~f:(function
                   | `Date x
                   | `Time x -> x) )
    )
    (s "FO" x.flow_order)
    (s "KS" x.key_seq)
    (s "LB" x.library)
    (s "PG" x.program)
    (s "PI" (Option.map x.predicted_median_insert_size ~f:Int.to_string))
    (s "PL" (Option.map x.platform ~f:print_platform))
    (s "PU" x.platform_unit)
    (s "SM" x.sample)

let print_program (x:program) =
  let s tag value = match value with
    |  None -> ""
    | Some x -> sprintf "\t%s:%s" tag x
  in
  sprintf "@PG\tID:%s%s%s%s%s%s"
    x.id
    (s "PN" x.name)
    (s "CL" x.command_line)
    (s "PP" x.previous_id)
    (s "DS" x.description)
    (s "VN" x.version)

let print_other ((tag,l) : string * tag_value list) =
  sprintf "@%s%s"
    tag
    (
      List.map l ~f:(fun (x,y) -> sprintf "\t%s:%s" x y)
      |> String.concat ~sep:""
    )


(******************************************************************************)
(* Alignment Printers                                                         *)
(******************************************************************************)
let print_qname = function Some x -> x | None -> "*"
let print_flags = Int.to_string
let print_rname = function Some x -> x | None -> "*"
let print_pos = function Some x -> Int.to_string x | None -> "0"
let print_mapq = function Some x -> Int.to_string x | None -> "255"

let print_cigar_op = function
  | `Alignment_match x -> sprintf "%dM" x
  | `Insertion x -> sprintf "%dI" x
  | `Deletion x -> sprintf "%dD" x
  | `Skipped x -> sprintf "%dN" x
  | `Soft_clipping x -> sprintf "%dS" x
  | `Hard_clipping x -> sprintf "%dH" x
  | `Padding x -> sprintf "%dP" x
  | `Seq_match x -> sprintf "%d=" x
  | `Seq_mismatch x -> sprintf "%dX" x

let print_cigar = function
  | [] -> "*"
  | cigar_ops ->
    List.map cigar_ops ~f:print_cigar_op
    |> String.concat ~sep:""

let print_rnext = function
  | None -> "*"
  | Some `Equal_to_RNAME -> "="
  | Some (`Value x) -> x

let print_pnext = function Some x -> Int.to_string x | None -> "0"
let print_tlen = function Some x -> Int.to_string x | None -> "0"
let print_seq = function Some x -> x | None -> "*"

let print_qual = function
  | [] -> "*"
  | quals ->
    List.map quals ~f:(fun x ->
      ok_exn (Phred_score.to_char ~offset:`Offset33 x)
    )
    |> String.of_char_list

let print_optional_field (x:optional_field) =
  let typ,value = match x.value with
    | `A x -> 'A', Char.to_string x
    | `i x -> 'i', Int64.to_string x
    | `f x -> 'f', Float.to_string x
    | `Z x -> 'Z', x
    | `H x -> 'H', x
    | `B (c,l) -> 'B', (String.concat ~sep:"," ((String.of_char c)::l))
  in
  sprintf "%s:%c:%s" x.tag typ value

let print_alignment a =
  sprintf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s"
    (print_qname a.qname)
    (print_flags a.flags)
    (print_rname a.rname)
    (print_pos a.pos)
    (print_mapq a.mapq)
    (print_cigar a.cigar)
    (print_rnext a.rnext)
    (print_pnext a.pnext)
    (print_tlen a.tlen)
    (print_seq a.seq)
    (print_qual a.qual)
    (
      List.map a.optional_fields ~f:print_optional_field
      |> String.concat ~sep:"\t"
    )

let read_header ic =
  parse_header_gen (fun () -> In_channel.input_line ic)

let write_header oc (h : header) =
  Option.iter h.version ~f:(fun version ->
      Out_channel.output_string oc (print_header_line {version; sort_order=h.sort_order; group_order=h.group_order}) ;
      Out_channel.output_char oc '\n'
  ) ;
  List.iter h.ref_seqs ~f:(fun x ->
      Out_channel.output_string oc (print_ref_seq x) ;
      Out_channel.output_char oc '\n'
    ) ;
  List.iter h.read_groups ~f:(fun x ->
      Out_channel.output_string oc (print_read_group x) ;
      Out_channel.output_char oc '\n'
    ) ;
  List.iter h.programs ~f:(fun x ->
      Out_channel.output_string oc (print_program x) ;
      Out_channel.output_char oc '\n'
    ) ;
  List.iter h.comments ~f:(fun x ->
      Out_channel.output_string oc "@CO\t" ;
      Out_channel.output_string oc x ;
      Out_channel.output_char oc '\n'
    ) ;
  List.iter h.others ~f:(fun x ->
      Out_channel.output_string oc (print_other x) ;
      Out_channel.output_char oc '\n'
    )

let write_alignment oc a =
  Out_channel.output_string oc (print_alignment a) ;
  Out_channel.output_char oc '\n'

let write_file ?perm ?append file ?header alignments =
  Out_channel.with_file ?perm ?append file ~f:(fun oc ->
      Option.iter header ~f:(write_header oc) ;
      Seq.iter (write_alignment oc) alignments
    )

let with_file fn ~f =
  let open Let_syntax.Result in
  let exception Escape of int * string in
  In_channel.with_file fn ~f:(fun ic ->
      let* (header, header_lno, maybe_next_line) = read_header ic in
      let als =
        let rec loop i = function
          | None -> Seq.empty
          | Some line ->
            match parse_alignment line with
            | Ok al ->
              Seq.cons al (loop (i + 1) (In_channel.input_line ic))
            | Error (`Msg s) -> raise (Escape (i, s))
        in
        loop (header_lno + 1) maybe_next_line
      in
      try Ok (f header als)
      with Escape (lno, msg) -> Error (`SAM_parsing_error (msg, Some lno))
      (* This reallocation is necesassary to enjoy full polymorphism *)
    )

let fold fn ~init ~f =
  with_file fn ~f:(fun header als ->
      Seq.fold_left
        (fun acc al -> f acc header al)
        (init header)
        als
    )
