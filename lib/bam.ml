open Core
open Prelude
open Let_syntax.Result

let check b msg =
  if b then Ok ()
  else Error.msg msg

let checkf b format = Printf.ksprintf (check b) format

let check_buf ~buf ~pos ~len =
  check (String.length buf >= pos + len) "Buffer too short"

(* Helper functions to get parts of an Int32.t as an int *)
let get_8_0 =
  let mask = Int32.of_int_exn 0xff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

let get_16_0 =
  let mask = Int32.of_int_exn 0xffff in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

let get_16_8 x =
  get_8_0 (Int32.shift_right x 8)

let get_32_4 x =
  Int32.shift_right x 4 |> Int32.to_int_exn

let get_4_0 =
  let mask = Int32.of_int_exn 0xf in
  fun x -> Int32.bit_and mask x |> Int32.to_int_exn

(* this is adapted from JaneStreet's core_kernel Binary_packing
   module in order to read from strings instead of bytes *)
module BP = struct
  let unpack_unsigned_8 ~buf ~pos = Char.to_int (String.get buf pos)

  let unpack_signed_8 ~buf ~pos =
    let n = unpack_unsigned_8 ~buf ~pos in
    if n >= 0x80 then -(0x100 - n) else n

  let unpack_signed_32_little_endian ~buf ~pos =
    let b1 =
      Int32.shift_left
        (Int32.of_int_exn (Stdlib.Char.code (String.get buf (pos + 3))))
        24
    in
    let b2 = Char.to_int (String.get buf (pos + 2)) lsl 16 in
    let b3 = Char.to_int (String.get buf (pos + 1)) lsl 8 in
    let b4 = Char.to_int (String.get buf pos) in
    (* LSB *)
    Int32.bit_or b1 (Int32.of_int_exn (b2 lor b3 lor b4))

  let unpack_unsigned_16_little_endian ~buf ~pos =
    let b1 = Char.to_int (String.get buf (pos + 1)) lsl 8 in
    let b2 = Char.to_int (String.get buf pos) in
    b1 lor b2

  let unpack_signed_16_little_endian ~buf ~pos =
    let n = unpack_unsigned_16_little_endian ~buf ~pos in
    if n >= 0x8000 then -(0x10000 - n) else n

  (* NB: uint32 is unpacked as an int64 *)
  let unpack_unsigned_32_little_endian ~buf ~pos =
    let b1 = Char.to_int (String.get buf (pos + 0)) |> Int64.of_int_exn in
    let b2 = Char.to_int (String.get buf (pos + 1)) in
    let b3 = Char.to_int (String.get buf (pos + 2)) in
    let b4 = Char.to_int (String.get buf (pos + 3)) in
    Int64.bit_or
      (Int64.shift_left b1 24)
      (Int64.of_int_exn (b2 lor b3 lor b4))

  let unpack_signed_64_little_endian ~buf ~pos =
    Int64.bit_or
      (Int64.bit_or
         (Int64.shift_left
            (Int64.of_int_exn (
                ((Char.to_int (String.get buf (pos + 7)) lsl 16))
                lor (Char.to_int (String.get buf (pos + 6)) lsl 8)
                lor Char.to_int (String.get buf (pos + 5))))
            40)
         (Int64.shift_left
            (Int64.of_int_exn
               ((Char.to_int (String.get buf (pos + 4)) lsl 16)
                lor (Char.to_int (String.get buf (pos + 3)) lsl 8)
                lor Char.to_int (String.get buf (pos + 2))))
            16))
      (Int64.of_int_exn (
          (Char.to_int (String.get buf (pos + 1)) lsl 8) lor Char.to_int (String.get buf pos)))

  let unpack_float_little_endian ~buf ~pos =
    Int64.float_of_bits (unpack_signed_64_little_endian ~buf ~pos)

  let pack_signed_32_little_endian bits =
    let buf = Bytes.create 4 in
    Binary_packing.pack_signed_32 ~byte_order:`Little_endian bits ~buf ~pos:0 ;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf

  let pack_u32_in_int64_little_endian v =
    let buf = Bytes.create 4 in
    (* Safely set the first and last bytes, so that we verify the string bounds. *)
    Bytes.set buf 0 (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL v))) ;
    Bytes.set buf 3 (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 24)))) ;
    (* Now we can use [unsafe_set] for the intermediate bytes. *)
    Bytes.unsafe_set buf 1
      (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 8)))) ;
    Bytes.unsafe_set buf 2
      (Char.unsafe_of_int Int64.(to_int_exn (bit_and 0xFFL (shift_right_logical v 16)))) ;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
end


let int64_is_neg n =
  Int64.(bit_and 0x8000000000000000L n <> zero)

let int64_fits_u32 n =
  Int64.(bit_and 0xFFFFFFFF00000000L n = zero)

let int64_fits_u31 n =
  Int64.(bit_and 0xFFFFFFFF80000000L n = zero)

let int64_fits_s32 n =
  if int64_is_neg n then
    int64_fits_u31 (Int64.bit_not n)
  else
    int64_fits_u31 n

(*
   A List.init with possibly failing initializer

   val result_list_init : int -> f:(int -> ('a, 'b) result) -> ('a list, 'b) result
*)
let result_list_init n ~f =
  let rec aux i accu =
    if i < 0 then Ok accu
    else (
      match f i with
      | Ok y -> aux (i - 1) (y :: accu)
      | Error _ as e -> e
    )
  in
  aux (n - 1) []

module Header = struct
  (* This type definition is essentially identical to the Sam version
     but makes parsing faster because in Bam, the seq name of a read
     is stored as an int index in the ref_seq array. *)
  type t = {
    ref_seq : Sam.ref_seq array ;
    sam_header : Sam.header ;
  }

  let to_sam h = h.sam_header
  let of_sam sam_header = {
    ref_seq = Array.of_list sam_header.Sam.ref_seqs ;
    sam_header
  }

end

open Header

type alignment = Sam.alignment

module Alignment0 = struct
  type t = {
    ref_id : int ;
    pos : int ;
    bin_mq_nl : int32 ;
    flag_nc : int32 ;
    next_ref_id : int ;
    pnext : int ;
    tlen : int ;
    read_name : string ;
    cigar : string ;
    seq : string ; (* compressed representation *)
    qual : string ;
    optional : string ;
  } [@@deriving sexp]

  (* ============================ *)
  (* ==== ACCESSOR FUNCTIONS ==== *)
  (* ============================ *)

  (* option constructor for encodings where a special value of the
     input type (here it is [none]) plays the role of [None] *)
  let option ~none x =
    if Poly.(x = none) then None
    else Some x

  let ref_id al = option ~none:(- 1) al.ref_id

  let qname al = option ~none:"*" al.read_name
  (* default is indicated in note 1 of page 14 of the spec *)

  let flags al =
    let i =
      Int32.shift_right al.flag_nc 16
      |> Int32.to_int_exn
      (* because we are shifting right just before,
         Int32.to_int_exn cannot fail *)
    in
    Sam.Flags.of_int i
    |> Result.map_error ~f:(fun _ -> `Flag_out_of_range i)

  let rname al header =
    try Ok (Option.map (ref_id al) ~f:(fun id -> (Array.get header.ref_seq id).Sam.name))
    with _ -> Error.msg "Bam.Alignment0.rname: unknown ref_id"

  let pos al = option ~none:(- 1) al.pos

  let mapq al = option ~none:255 (get_16_8 al.bin_mq_nl)

  let cigar_op_of_s32 x =
    let open Sam in
    let op_len = get_32_4 x in
    match get_4_0 x with
    | 0 -> cigar_op_alignment_match op_len
    | 1 -> cigar_op_insertion op_len
    | 2 -> cigar_op_deletion op_len
    | 3 -> cigar_op_skipped op_len
    | 4 -> cigar_op_soft_clipping op_len
    | 5 -> cigar_op_hard_clipping op_len
    | 6 -> cigar_op_padding op_len
    | 7 -> cigar_op_seq_match op_len
    | 8 -> cigar_op_seq_mismatch op_len
    | _ -> assert false

  let cigar al =
    result_list_init (String.length al.cigar / 4) ~f:(fun i ->
        let s32 = BP.unpack_signed_32_little_endian ~buf:al.cigar ~pos:(i * 4) in
        cigar_op_of_s32 s32
      )

  let rnext al header =
    match al.next_ref_id with
    | -1 -> Ok None
    | i -> Sam.parse_rnext header.ref_seq.(i).Sam.name

  let pnext al = option ~none:(- 1) al.pnext
  (* value for unavailable is the corresponding value in SAM - 1 *)

  let tlen al = option ~none:0 al.tlen

  let l_seq al = String.length al.qual

  let char_of_seq_code  = function
    | 0  -> '='
    | 1  -> 'A'
    | 2  -> 'C'
    | 3  -> 'M'
    | 4  -> 'G'
    | 5  -> 'R'
    | 6  -> 'S'
    | 7  -> 'V'
    | 8  -> 'T'
    | 9  -> 'W'
    | 10 -> 'Y'
    | 11 -> 'H'
    | 12 -> 'K'
    | 13 -> 'D'
    | 14 -> 'B'
    | 15 -> 'N'
    | l -> failwithf "letter not in [0, 15]: %d" l ()

  let seq_code_of_char  = function
    | '=' -> 0
    | 'A' -> 1
    | 'C' -> 2
    | 'M' -> 3
    | 'G' -> 4
    | 'R' -> 5
    | 'S' -> 6
    | 'V' -> 7
    | 'T' -> 8
    | 'W' -> 9
    | 'Y' -> 10
    | 'H' -> 11
    | 'K' -> 12
    | 'D' -> 13
    | 'B' -> 14
    | 'N' -> 15
    | c -> failwithf "invalid character: %c" c ()

  let decode_seq l_seq seq =
    if l_seq = 0 then None
    else
      let r = Bytes.make l_seq ' ' in
      for i = 0 to String.length seq - 1 do
        let c = int_of_char seq.[i] in
        Bytes.set r (2 * i) (char_of_seq_code (c lsr 4)) ;
        if 2 * i + 1 < l_seq then Bytes.set r (2 * i + 1) (char_of_seq_code (c land 0xf)) ;
      done ;
      Some (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:r)

  let encode_seq seq =
    let l_seq = String.length seq in
    let extra = l_seq mod 2 = 1 in
    let n = l_seq / 2 + if extra then 1 else 0 in
    let r = Bytes.make n '\000' in
    for i = 0 to l_seq / 2 - 1 do
      let c1 = seq_code_of_char seq.[2 * i] lsl 4 in
      let c2 = seq_code_of_char seq.[2 * i + 1] in
      Bytes.set r i (Stdlib.char_of_int (c1 lor c2))
    done ;
    if extra then (
      Bytes.set r (n - 1) (Stdlib.char_of_int (seq_code_of_char seq.[l_seq - 1] lsl 4))
    ) ;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:r

  let seq al =
    let l_seq = l_seq al in
    decode_seq l_seq al.seq

  let qual al =
    let shift = String.map ~f:Char.(fun c -> of_int_exn (to_int c + 33)) in
    match shift al.qual with
    | qual33 -> Sam.parse_qual qual33
    | exception Failure _ ->
      Error.msgf
        "Bam.Alignement0.qual: incorrect quality score (%s)"
        al.qual

  (** Extracts string in buf starting from pos and finishing with a
      NULL character, and returns the position just after it. Returns
      an error if no NULL character is encountered before the end of
      the string. *)
  let parse_cstring buf pos =
    let rec aux i =
      if i < String.length buf then
        if Char.(String.get buf i = '\000') then Ok i
        else aux (i + 1)
      else Error.msg "Unfinished NULL terminated string"
    in
    let+ pos' = aux pos in
    String.sub buf ~pos ~len:(pos' - pos), pos' + 1

  let cCsSiIf_size = function
    | 'c'
    | 'C' -> Ok 1
    | 's' | 'S' -> Ok 2
    | 'i' | 'I'
    | 'f' -> Ok 4
    | _ -> Error.msg "Incorrect numeric optional field type identifier"

  let parse_cCsSiIf buf pos typ =
    let* len = cCsSiIf_size typ in
    let* () = check_buf ~buf ~pos ~len in
    match typ with
    | 'c' ->
      let i = Int64.of_int_exn (BP.unpack_signed_8 ~buf ~pos) in
      Ok (Sam.optional_field_value_i i, len)
    | 'C' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_8 ~buf ~pos) in
      Ok (Sam.optional_field_value_i i, len)
    | 's' ->
      let i = Int64.of_int_exn (BP.unpack_signed_16_little_endian ~buf ~pos) in
      Ok (Sam.optional_field_value_i i, len)
    | 'S' ->
      let i = Int64.of_int_exn (BP.unpack_unsigned_16_little_endian ~buf ~pos) in
      Ok (Sam.optional_field_value_i i, len)
    | 'i' ->
      let i = BP.unpack_signed_32_little_endian ~buf ~pos in
      Ok (Sam.optional_field_value_i (Int64.of_int32 i), len)
    | 'I' ->
      let i = BP.unpack_unsigned_32_little_endian ~buf ~pos in
      Ok (Sam.optional_field_value_i i, len)
    | 'f' ->
      let f = BP.unpack_float_little_endian ~buf ~pos in
      Ok (Sam.optional_field_value_f f, len)
    | _ -> Error.msg "Incorrect numeric optional field type identifier"

  let parse_optional_field_value buf pos = function
    | 'A' ->
      let* () = check_buf ~buf ~pos ~len:1 in
      let+ v = Sam.optional_field_value_A (String.get buf pos) in
      (v, 1)
    | 'c' | 'C' | 's' | 'S' | 'i' | 'I' | 'f' as typ ->
      parse_cCsSiIf buf pos typ
    | 'Z' ->
      let* s, pos' = parse_cstring buf pos in
      let+ value = Sam.optional_field_value_Z s in
      value, pos' - pos
    | 'H' ->
      let* s, pos' = parse_cstring buf pos in
      let+ value = Sam.optional_field_value_H s in
      value, pos' - pos
    | 'B' ->
      let* () = check_buf ~buf ~pos ~len:5 in
      let typ = String.get buf 0 in
      let n = BP.unpack_signed_32_little_endian ~buf ~pos:(pos + 1) in
      (
        match Int32.to_int n with
        | Some n ->
          let* elt_size = cCsSiIf_size typ in
          let* () = check_buf ~buf ~pos:(pos + 5) ~len:(n * elt_size) in
          let elts = List.init n ~f:(fun i -> String.sub buf ~pos:(pos + 5 + i * elt_size) ~len:elt_size) in
          let bytes_read = 5 (* array type and size *) + elt_size * n in
          let+ value = Sam.optional_field_value_B typ elts in
          value, bytes_read
        | None ->
          Error.msg "Too many elements in B-type optional field"
      )
    | c -> Error.msgf "Incorrect optional field type identifier (%c)" c

  let parse_optional_field buf pos =
    let* () = check_buf ~buf ~pos ~len:3 in
    let tag = String.sub buf ~pos ~len:2 in
    let field_type = buf.[pos + 2] in
    let* field_value, shift =
      parse_optional_field_value buf (pos + 3) field_type
    in
    let+ field = Sam.optional_field tag field_value in
    field, shift + 3

  let parse_optional_fields buf =
    let rec loop buf pos accu =
      if pos = String.length buf then Ok (List.rev accu)
      else
        let* field, used_chars = parse_optional_field buf pos in
        loop buf (pos + used_chars) (field :: accu)
    in
    loop buf 0 []

  let optional_fields al =
    parse_optional_fields al.optional

  (* ============================ *)
  (* ==== ALIGNMENT DECODING ==== *)
  (* ============================ *)

  (* Alignement0.t -> Alignment.t conversion *)
  let decode al header =
    let* flags =
      flags al
      |> Result.map_error ~f:(fun (`Flag_out_of_range i) ->
          `Msg (sprintf "flag out of range (%d)" i)
        )
    in
    let* rname = rname al header in
    let* cigar = cigar al in
    let* rnext = rnext al header in
    let* qual = qual al in
    let* optional_fields = optional_fields al in
    Sam.alignment
      ?qname:(qname al) ~flags ?rname ?pos:(pos al) ?mapq:(mapq al)
      ~cigar ?rnext ?pnext:(pnext al) ?tlen:(tlen al)
      ?seq:(seq al) ~qual ~optional_fields
      ()

  (* ============================ *)
  (* ==== ALIGNMENT ENCODING ==== *)
  (* ============================ *)

  (* Alignment.t -> Alignment0.t conversion *)
  let find_ref_id header ref_name =
    match Array.findi header.ref_seq ~f:(fun _ rs -> String.(rs.Sam.name = ref_name)) with
    | Some (i, _) -> Ok i
    | None  -> Error.msg "Bam: unknown reference id"


  let string_of_cigar_ops cigar_ops =
    let buf = Bytes.create (List.length cigar_ops * 4) in
    let write ith i32 =
      let pos = ith * 4 in
      Binary_packing.pack_signed_32 ~byte_order:`Little_endian ~buf ~pos i32 in
    let open Int32 in
    List.iteri cigar_ops ~f:(fun idx op ->
        let _, i = match op with
          | `Alignment_match  i -> 0l, i
          | `Insertion i -> 1l, i
          | `Deletion i -> 2l, i
          | `Skipped i -> 3l, i
          | `Soft_clipping i -> 4l, i
          | `Hard_clipping i -> 5l, i
          | `Padding i -> 6l, i
          | `Seq_match i -> 7l, i
          | `Seq_mismatch i -> 8l, i
        in
        write idx (bit_or 0l (of_int_exn Stdlib.(i lsl 4)))
      ) ;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf

  let sizeof al =
    let l_seq = l_seq al in
    8 * 4
    + String.length al.read_name + 1 (* NULL terminated string *)
    + String.length al.cigar
    + (l_seq + 1) / 2
    + l_seq
    + String.length al.optional


  (* supposes interval closed at both ends *)
  let reg2bin st ed =
    match st, ed with
    | b, e when b lsr 14 = e lsr 14 ->
      ((1 lsl 15) - 1) / 7  +  (st lsr 14)
    | b, e when b lsr 17 = e lsr 17 ->
      ((1 lsl 12) - 1) / 7  +  (st lsr 17)
    | b, e when b lsr 20 = e lsr 20 ->
      ((1 lsl 9) - 1) / 7  +  (st lsr 20)
    | b, e when b lsr 23 = e lsr 23 ->
      ((1 lsl 6) - 1) / 7  +  (st lsr 23)
    | b, e when b lsr 26 = e lsr 26 ->
      ((1 lsl 3) - 1) / 7  +  (st lsr 26)
    | _ -> 0


  let string_of_optional_fields opt_fields =

    let field_value_encoding = function
      | `B (typ, xs) ->
        Ok ('B',
            sprintf "%c%s" typ (String.concat ~sep:"" xs))

      | `A c ->
        Ok ('A', Char.to_string c)

      | `f f ->
        let bits = Int32.bits_of_float f in
        Ok ('f', BP.pack_signed_32_little_endian bits)

      | `i i ->
        (* FIXME: encode i to the smallest usable integer type *)
        if int64_fits_u32 i then (
          Ok ('I', BP.pack_u32_in_int64_little_endian i)
        )
        else if int64_fits_s32 i then (
          Ok ('i', BP.pack_signed_32_little_endian (Int32.of_int64_exn i))
        )
        else Error.msgf "Sam integer cannot be encoded in BAM format (%Ld)" i

      | `H s -> (
          let r = ref [] in
          String.iter s ~f:(fun c ->
              r := sprintf "%02x" (Char.to_int c) :: !r
            ) ;
          Ok ('H',
              String.concat ~sep:"" (List.rev !r) ^ "\000")
        )

      | `Z s ->
        Ok ('Z', s ^ "\000")
    in
    List.map opt_fields ~f:(fun opt_field ->
        let+ c, s = field_value_encoding opt_field.Sam.value in
        sprintf "%s%c%s" opt_field.Sam.tag c s
      )
    |> Result.all
    |> Result.map ~f:(String.concat ~sep:"")

  let int32 i ~ub var =
    if i < ub then
      match Int32.of_int i with
      | Some i -> Ok i
      | None -> Error.msg "invalid conversion to int32"
    else
      Error.msgf "invalid conversion to int32 (%s than %d)" var ub

  let encode_bin_mq_nl ~bin ~mapq ~l_read_name =
    let open Int32 in
    let* bin = int32 bin ~ub:65536 "bin" in
    let* mapq = int32 mapq ~ub:256 "mapq" in
    let+ l_read_name = int32 l_read_name ~ub:256 "l_read_name" in
    bit_or
      (shift_left bin 16)
      (bit_or (shift_left mapq 8) l_read_name)

  let encode_flag_nc ~flags ~n_cigar_ops =
    let open Int32 in
    let* flags = int32 flags ~ub:65536 "flags" in
    let+ n_cigar_ops = int32 n_cigar_ops ~ub:65536 "n_cigar_ops" in
    bit_or (shift_left flags 16) n_cigar_ops

  let encode al header =
    let* ref_id =
      match al.Sam.rname with
      | Some rname -> find_ref_id header rname
      | None -> Ok (-1)
    in
    let read_name = Option.value ~default:"*" al.Sam.qname in
    let seq = Option.value_map ~default:"" ~f:encode_seq al.Sam.seq in

    let pos = (Option.value ~default:0 al.Sam.pos) - 1 in

    let bin = reg2bin pos (pos + String.(length seq)) in
    let mapq = Option.value ~default:255 al.Sam.mapq in
    let l_read_name = String.length read_name + 1 in (* NULL terminated string *)
    let* bin_mq_nl = encode_bin_mq_nl ~bin ~mapq ~l_read_name in
    let flags = (al.Sam.flags :> int) in
    let n_cigar_ops = List.length al.Sam.cigar in
    let* flag_nc = encode_flag_nc ~flags ~n_cigar_ops in
    let* next_ref_id =
      match al.Sam.rnext with
      | Some `Equal_to_RNAME -> Ok ref_id
      | Some (`Value s) -> find_ref_id header s
      | None -> Ok (-1)
    in
    let pnext = Option.value ~default:0 al.Sam.pnext - 1 in
    let tlen = Option.value ~default:0 al.Sam.tlen in
    let cigar = string_of_cigar_ops al.Sam.cigar in
    let* qual =
      List.map al.Sam.qual ~f:(fun c ->
          Phred_score.to_char ~offset:`Offset33 c
          |> Result.map_error ~f:(fun _ -> `Msg "Invalid phred score")
        )
      |> Result.all
      |> Result.map ~f:String.of_char_list
    in
    let+ optional =
      string_of_optional_fields al.Sam.optional_fields
    in
    {
      ref_id; pos; bin_mq_nl; flag_nc; cigar;
      next_ref_id; pnext; tlen; seq; qual; optional;
      read_name
    }

end

let magic_string = "BAM\001"

let input_s32_as_int iz =
  Int32.to_int_exn (Bgzf.input_s32 iz)

let read_sam_header iz =
  try
    let magic = Bgzf.input_string iz 4 in
    let* () = check String.(magic = magic_string) "Incorrect magic string, not a BAM file" in
    let l_text = input_s32_as_int iz in
    let* () = check (l_text >= 0) "Incorrect size of plain text in BAM header" in
    let text = Bgzf.input_string iz l_text in
    Sam.parse_header text
    |> Result.map_error ~f:(fun (`SAM_parsing_error (msg, _)) ->
        `Msg msg
      )
  with End_of_file -> Error.msg "EOF while reading BAM header"

let read_one_reference_information iz =
  try
    let l_name = input_s32_as_int iz in
    let* () = check (l_name > 0) "Incorrect encoding of reference sequence name in BAM header" in
    let name = Bgzf.input_string iz (l_name - 1) in
    let (_ : char) = Bgzf.input_char iz in (* name is a NULL terminated string *)
    let length = input_s32_as_int iz in
    Sam.ref_seq ~name ~length ()
  with End_of_file -> Error.msg "EOF while reading BAM reference information"

let read_reference_information iz =
  let rec loop accu n =
    if n = 0 then Ok (List.rev accu)
    else
      match read_one_reference_information iz with
      | Ok refseq -> loop (refseq :: accu) (n - 1)
      | Error _ as e -> e
  in
  try
    let n_ref = input_s32_as_int iz in
    let+ refs = loop [] n_ref in
    Array.of_list refs
  with End_of_file -> Error.msg "EOF while reading BAM reference information"

let read_alignment_aux iz block_size =
  try
    let ref_id = input_s32_as_int iz in
    let* pos =
      match Bgzf.input_s32 iz |> Int32.to_int with
      | Some 2147483647 (* POS in BAM is 0-based *)
      | None -> Error.msg "A read has a position greater than 2^31"
      | Some n ->
        if n < -1 then Error.msgf "A read has a negative position %d" n
        else Ok n
    in
    let bin_mq_nl = Bgzf.input_s32 iz in
    let l_read_name = get_8_0 bin_mq_nl in
    let* () = checkf (l_read_name > 0) "Alignment with l_read_name = %d"  l_read_name in
    let flag_nc = Bgzf.input_s32 iz in
    let n_cigar_ops = get_16_0 flag_nc in
    let l_seq = input_s32_as_int iz in
    let* () = check (l_seq >= 0) "Incorrect sequence length in alignment" in
    let next_ref_id = input_s32_as_int iz in
    let+ pnext =
      match Bgzf.input_s32 iz |> Int32.to_int with
      | Some 2147483647
      | None -> Error.msg "A read has a position > than 2^31"
      | Some n ->
        if n < -1 then Error.msgf "A read has a negative next position %d" n
        else Ok n
    in
    let tlen = input_s32_as_int iz in
    let read_name =
      let r = Bgzf.input_string iz (l_read_name - 1) in
      let (_ : char) = Bgzf.input_char iz in (* trailing null character *)
      r
    in
    let cigar = Bgzf.input_string iz (n_cigar_ops * 4) in
    let seq = Bgzf.input_string iz ((l_seq + 1) / 2) in
    let qual = Bgzf.input_string iz l_seq in
    let remaining = block_size - 32 - l_read_name - 4 * n_cigar_ops - ((l_seq + 1) / 2) - l_seq in
    let optional = Bgzf.input_string iz remaining in
    {
      Alignment0.ref_id ; read_name ; flag_nc ; pos ; bin_mq_nl ; cigar ;
      next_ref_id ; pnext ; tlen ; seq ; qual ; optional
    }
  with End_of_file -> Error.msg "EOF while reading alignment"

let read_alignment iz =
  try
    let block_size = input_s32_as_int iz in
    Some (read_alignment_aux iz block_size)
  with End_of_file -> None

let read_alignment_stream iz =
  let rec loop () =
    match read_alignment iz with
    | None -> Seq.Nil
    | Some al_or_error -> Seq.Cons (al_or_error, loop)
  in
  loop

let read_header iz =
  let* sam_header = read_sam_header iz in
  let+ ref_seq = read_reference_information iz in
  { sam_header ; ref_seq }

let read0 ic =
  let iz = Bgzf.of_in_channel ic in
  let+ header = read_header iz in
  (header, read_alignment_stream iz)


let with_file0 fn ~f =
  let exception Escape of string in
  In_channel.with_file ~binary:true fn ~f:(fun ic ->
      let* header, al_or_errors = read0 ic in
      let alignments =
        Seq.map
          (function
            | Ok al -> al
            | Error (`Msg msg) -> raise (Escape msg))
          al_or_errors
      in
      try Ok (f header alignments)
      with Escape msg -> Error.msg msg
    )

let write_plain_SAM_header h oz =
  let open Sam in
  let buf = Buffer.create 1024 in
  let add_line x =
    Buffer.add_string buf x ;
    Buffer.add_char buf '\n'
  in
  Option.iter h.version ~f:(fun version ->
      let hl =
        header_line ~version ?sort_order:h.sort_order ()
        |> ok_or_failwith
      in (* the construction of the header line must be valid since we are building it from a validated header *)
      add_line (print_header_line hl)
    ) ;
  List.iter h.ref_seqs ~f:(fun x ->
      add_line (print_ref_seq x)
    ) ;
  List.iter h.read_groups ~f:(fun x ->
      add_line (print_read_group x)
    ) ;
  List.iter h.programs ~f:(fun x ->
      add_line (print_program x)
    ) ;
  List.iter h.comments ~f:(fun x ->
      Buffer.add_string buf "@CO\t" ;
      add_line x
    ) ;
  List.iter h.others ~f:(fun x ->
      add_line (print_other x)
    ) ;
  Bgzf.output_s32 oz (Int32.of_int_exn (Buffer.length buf)) ; (* safe conversion of int32 to int: SAM headers less than a few KB *)
  Bgzf.output_string oz (Buffer.contents buf)

let output_null_terminated_string oz s =
  Bgzf.output_string oz s ;
  Bgzf.output_char oz '\000'

let write_reference_sequences h oz =
  let open Sam in
  Bgzf.output_s32 oz (Int32.of_int_exn (Array.length h.ref_seq)) ; (* safe conversion: more than a few million reference sequences cannot happen in practice *)
  Array.iter h.ref_seq ~f:(fun rs ->
      Bgzf.output_s32 oz (Int32.of_int_exn (String.length rs.name + 1)) ; (* safe conversion: the length of the name of a reference sequence is shorter than a few hundreds *)
      output_null_terminated_string oz rs.name ;
      Bgzf.output_s32 oz (Int32.of_int_exn rs.length) ; (* FIXME: the conversion is possibly not safe, but maybe [Sam.ref_seq] type should keep the int32 representation? *)
    )

let write_header oz header =
  Bgzf.output_string oz magic_string ;
  write_plain_SAM_header header.sam_header oz  ;
  write_reference_sequences header oz

let write_alignment0 oz al =
  let open Alignment0 in
  Bgzf.output_s32 oz (Int32.of_int_exn (sizeof al)) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.ref_id) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.pos) ;
  Bgzf.output_s32 oz al.bin_mq_nl ;
  Bgzf.output_s32 oz al.flag_nc ;
  Bgzf.output_s32 oz (Int32.of_int_exn (l_seq al)) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.next_ref_id) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.pnext) ;
  Bgzf.output_s32 oz (Int32.of_int_exn al.tlen) ;
  output_null_terminated_string oz al.read_name ;
  Bgzf.output_string oz al.cigar ;
  Bgzf.output_string oz al.seq ;
  Bgzf.output_string oz al.qual ;
  Bgzf.output_string oz al.optional

let write0 header alignments oc =
  let oz = Bgzf.of_out_channel oc in
  write_header oz header ;
  Seq.iter (write_alignment0 oz) alignments ;
  Bgzf.dispose_out oz

let bind f x = Result.bind x ~f

let read ic =
  let+ (header, xs) = read0 ic in
  (header, Seq.map (bind (fun r -> Alignment0.decode r header)) xs)

let with_file fn ~f =
  let exception Escape of string in
  with_file0 fn ~f:(fun header xs ->
      let als =
        Seq.map
          (fun al0 ->
             match Alignment0.decode al0 header with
             | Ok al -> al
             | Error (`Msg msg) -> raise (Escape msg))
          xs
      in
      try Ok (f header als)
      with Escape msg -> Error.msg msg
    )
  |> Result.join

let write h xs oc =
  let exception E of string in
  let xs = Seq.map (fun al ->
      match Alignment0.encode al h with
      | Ok r -> r
      | Error (`Msg e) -> raise (E e)
    ) xs
  in
  try write0 h xs oc ; Ok ()
  with E e -> Error.msg e
