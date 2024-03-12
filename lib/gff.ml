open Core

module Record = struct
  type t = {
    seqname    : string ;
    source     : string option ;
    feature    : string option ;
    start_pos  : int ;
    stop_pos   : int ;
    score      : float option ;
    strand     : [`Plus | `Minus | `Not_stranded | `Unknown ] ;
    phase      : int option ;
    attributes : (string * string list) list ;
  }
  [@@deriving sexp]

  let loc r = GLoc.{ chr = r.seqname ; lo = r.start_pos ; hi = r.stop_pos }
  let length r = r.stop_pos - r.start_pos + 1
  let attribute_exn r k =
    List.Assoc.find_exn ~equal:String.equal r.attributes k
    |> List.hd_exn
end

let record
    ?source ?feature ?score ?(strand = `Unknown) ?phase ?(attributes = [])
    seqname start_pos stop_pos =
  {
    Record.seqname ; source ; feature ; start_pos ; stop_pos ; score ; strand ; phase ; attributes ;
  }

type item = [
  | `Comment of string
  | `Record of Record.t
]

module Handwritten_parser = struct
  let fail s = Error (`Msg s)
  let failf fmt = Printf.ksprintf fail fmt

  let parse_float s =
    try Ok (Float.of_string s)
    with Failure s -> fail s

  let parse_int s =
    try Ok (Int.of_string s)
    with Failure s -> fail s

  let parse_opt f = function
    | "." -> None
    | s -> Some (f s)

  let parse_opt_err f = function
    | "." -> Ok None
    | s ->
      Result.(f s >>| Option.some)

  let parse_strand = function
    | "." -> Ok `Not_stranded
    | "?" -> Ok `Unknown
    | "+" -> Ok `Plus
    | "-" -> Ok `Minus
    | _ -> Error (`Msg "Incorrect strand character")

  let parse_tag pos buf =
    match String.index_from buf pos '=' with
    | None -> fail "Tag without a value"
    | Some k ->
      Ok (k + 1, String.sub buf ~pos ~len:(k - pos))

  let%test "Gff.parse_tag" =
    Stdlib.(parse_tag 0 "gene_id=foo" = Ok (8, "gene_id"))

  let lfind_mapi ?(pos = 0) s ~f =
    let n = String.length s in
    let rec loop i =
      if i < n then
        match f i s.[i] with
        | None -> loop (i + 1)
        | Some y -> Some y
      else
        None
    in
    loop pos

  let rec parse_value_list pos buf acc =
    let comma_or_semi_colon i = function
      | ',' -> Some (i, `Comma)
      | ';' -> Some (i, `Semi_colon)
      | _ -> None
    in
    match lfind_mapi ~pos buf ~f:comma_or_semi_colon with
    | None ->
      let n = String.length buf in
      let value = String.sub buf ~pos ~len:(n - pos) in
      n, List.rev (value :: acc)
    | Some (k, `Comma) ->
      let value = String.sub buf ~pos ~len:(k - pos) in
      parse_value_list (k + 1) buf (value :: acc)
    | Some (k, `Semi_colon) ->
      let value = String.sub buf ~pos ~len:(k - pos) in
      k + 1, List.rev (value :: acc)

  let test_parse_value buf y =
    let n, x = parse_value_list 0 buf [] in
    Stdlib.((n, x) = (String.length buf, y))

  let%test "parse_value_list1" =
    test_parse_value
      "region_id=chr1:3008683-3009183"
      ["region_id=chr1:3008683-3009183"]

  let%test "parse_value_list2" =
    test_parse_value "3,4" ["3" ; "4"]

  let rec parse_gff3_attributes pos buf acc =
    let open Result in
    if pos >= String.length buf then Ok (List.rev acc)
    else
      parse_tag pos buf >>= fun (pos, tag) ->
      let pos, values = parse_value_list pos buf [] in
      let acc = (tag, values) :: acc in
      parse_gff3_attributes pos buf acc

  let parse_gff3_attributes buf = parse_gff3_attributes 0 buf []

  let%test "Gff.parse_gff3_attributes" =
    Stdlib.(
      parse_gff3_attributes "a=2,3;b=4"
      =
      Ok [ ("a", ["2" ; "3"]) ; ("b", ["4"]) ]
    )

  let parse_gff2_attributes buf =
    let open Result.Monad_infix in
    let rec tokenize acc p =
      if p >= String.length buf then Ok (List.rev acc)
      else
        match buf.[p] with
        | '\t' -> fail "Unexpected tag character"
        | '\n' -> fail "Unexpected EOL character"
        | ' ' -> tokenize acc (p + 1)
        | ';' -> tokenize (`SEMICOLON :: acc) (p + 1)
        | '"' ->
          next_quote (p + 1) >>= fun q ->
          let len = q - p - 1 in
          tokenize (`QUOTED (p + 1, len) :: acc) (q + 1)
        | _ ->
          token_end p >>= fun q ->
          tokenize (`TOKEN (p, q - p + 1) :: acc) (q + 1)

    and next_quote p =
      if p >= String.length buf then fail "Reached end of string but expected dquote"
      else
        match buf.[p] with
        | '"' -> Ok p
        | _ -> next_quote (p + 1)

    and token_end p =
      if p >= String.length buf then Ok (p - 1)
      else
        match buf.[p] with
        | ' ' -> Ok (p - 1)
        | _ -> token_end (p + 1)
    in
    let rec attribute acc = function
      | `TOKEN (p, q) :: (`QUOTED (r, s) | `TOKEN (r, s)) :: rest ->
        let att = String.sub buf ~pos:p ~len:q, [ String.sub buf ~pos:r ~len:s ] in
        attribute_tail (att :: acc) rest
      | _ -> failf "Cannot parse attributes: %s" buf
    and attribute_tail acc = function
      | []
      | [`SEMICOLON] -> Ok (List.rev acc)
      | `SEMICOLON :: rest -> attribute acc rest
      | _ -> failf "Cannot parse attributes: %s" buf
    in
    tokenize [] 0 >>= fun tokens ->
    attribute [] tokens

  let%test "Gff.parse_gff2_attributes1" =
    Stdlib.(
      parse_gff2_attributes {|gene_id "FBgn0031081"|}
      =
      Ok [ ("gene_id", ["FBgn0031081"]) ]
    )

  let%test "Gff.parse_gff2_attributes" =
    Stdlib.(
      parse_gff2_attributes {|gene_id "FBgn0031081"; gene_symbol "Nep3"; transcript_id "FBtr0070000"; transcript_symbol "Nep3-RA";|}
      =
      Ok [ ("gene_id", ["FBgn0031081"]) ; ("gene_symbol", ["Nep3"]) ;
           ("transcript_id", ["FBtr0070000"]) ; ("transcript_symbol", ["Nep3-RA"]) ]
    )

  let parse_fields parse_attributes = function
    | [ seqname ; source ; feature ; start_pos ; stop_pos ;
        score ; strand ; phase ; attributes ] ->
      let open Result in
      parse_int start_pos >>= fun start_pos ->
      parse_int stop_pos >>= fun stop_pos ->
      parse_opt_err parse_int phase >>= fun phase ->
      parse_opt_err parse_float score >>= fun score ->
      parse_strand strand >>= fun strand ->
      parse_attributes attributes >>= fun attributes ->
      Ok {
        Record.seqname ;
        source = parse_opt Fn.id source ;
        feature = parse_opt Fn.id feature ;
        start_pos ;
        stop_pos ;
        score ;
        strand ;
        phase ;
        attributes ;
      }
    | _ -> fail "Incorrect number of fields"

  let parse_record version line =
    let parse_attributes = match version with
      | `gff2 -> parse_gff2_attributes
      | `gff3 -> parse_gff3_attributes
    in
    Line.split ~on:'\t' line
    |> parse_fields parse_attributes

  let unparse_record version (t : Record.t) =
    let escape =
      match version with
      | `gff2 -> (fun s -> Uri.pct_encode s)
      | `gff3 -> sprintf "%S"
    in
    let optescape o =  Option.value_map ~default:"." o ~f:escape in
    String.concat ~sep:"\t" [
      t.seqname ;
      optescape t.source ;
      Option.value ~default:"." t.feature ;
      Int.to_string t.start_pos ;
      Int.to_string t.stop_pos ;
      Option.value_map ~default:"." ~f:(sprintf "%g") t.score;
      (match t.strand with`Plus -> "+" | `Minus -> "-"
                        | `Not_stranded -> "." | `Unknown -> "?");
      Option.value_map ~default:"." ~f:(sprintf "%d") t.phase;
      String.concat ~sep:";"
        (List.map t.attributes ~f:(fun (k,v) ->
             match version with
             | `gff3 ->
               sprintf "%s=%s" (Uri.pct_encode k)
                 (List.map v ~f:Uri.pct_encode |> String.concat ~sep:",")
             | `gff2 ->
               sprintf "%s %s" k
                 (List.map v ~f:escape |> String.concat ~sep:",")
           ));
    ]

  let parse_item version line =
    match (line : Line.t :> string) with
    | "" -> fail "Empty line"
    | s ->
      if Char.(s.[0] = '#') then
        Ok (`Comment (String.sub s ~pos:1 ~len:(String.length s - 1)))
      else
        let open Let_syntax.Result in
        let+ r = parse_record version line in
        `Record r

  let unparse_item version : item -> string = function
    | `Comment c -> "#" ^ c
    | `Record r -> unparse_record version r

  let ensembl_gtf_example = {|1	transcribed_unprocessed_pseudogene	gene	11869	14409	.	+	.	gene_id "ENSG00000223972"; gene_name "DDX11L1"; gene_source "havana"; gene_biotype "transcribed_unprocessed_pseudogene";
1	processed_transcript	transcript	11869	14409	.	+	.	gene_id "ENSG00000223972"; transcript_id "ENST00000456328"; gene_name "DDX11L1"; gene_source "havana"; gene_biotype "transcribed_unprocessed_pseudogene"; transcript_name "DDX11L1-002"; transcript_source "havana";
|}

  let ensembl_gff_example = {|X	Ensembl	Repeat	2419108	2419128	42	.	.	hid=trf; hstart=1; hend=21
X	Ensembl	Repeat	2419108	2419410	2502	-	.	hid=AluSx; hstart=1; hend=303
X	Ensembl	Repeat	2419108	2419128	0	.	.	hid=dust; hstart=2419108; hend=2419128
X	Ensembl	Pred.trans.	2416676	2418760	450.19	-	2	genscan=GENSCAN00000019335
X	Ensembl	Variation	2413425	2413425	.	+	.	 #
X	Ensembl	Variation	2413805	2413805	.	+	.	 #
|}
end

module Angstrom_parser = struct
  (* http://gmod.org/wiki/GFF2 *)
  open Angstrom

  let field name =
    take_while1 (function
        | '\t' | '\n' -> false
        | _ -> true
      )
    <?> ("field:" ^ name)

  let to_int fieldnum s =
    match Int.of_string s with
    | n -> return n
    | exception _ ->
      fail (sprintf "failed to convert %s to integer at field %d" s fieldnum)

  let to_float fieldnum s =
    match Float.of_string s with
    | x -> return x
    | exception _ ->
      fail (sprintf "failed to convert %s to float at field %d" s fieldnum)

  let option = function
    | "." -> None
    | s -> Some s

  let maybe_convert f = function
    | "." -> return None
    | s -> f s >>| Option.some

  let strand fieldnum = function
    | "." -> return `Not_stranded
    | "?" -> return `Unknown
    | "+" -> return `Plus
    | "-" -> return `Minus
    | _ -> fail (sprintf "Incorrect strand character at field %d" fieldnum)

  let gff2_attribute_id = take_while1 (function
      | 'a'..'z' | 'A'..'Z'
      | '_' -> true
      | _ -> false
    )

  let gff2_quoted_attribute_value =
    char '"'
    *> take_while (function
        | '"' | '\n' -> false
        | _ -> true
      )
    <* char '"'
    >>| Scanf.unescaped

  let gff2_unquoted_attribute_value =
    take_while (function
        | ' ' | ';' | '\n' -> false
        | _ -> true
      )

  let gff2_attribute =
    gff2_attribute_id >>= fun id ->
    skip_many1 (char ' ') >>= fun () ->
    (gff2_quoted_attribute_value <|> gff2_unquoted_attribute_value) >>= fun s ->
    return (id, [ s ])

  let attribute_separator =
    char ';' >>= fun _ ->
    skip_many (char ' ')

  let gff2_attributes =
    sep_by attribute_separator gff2_attribute
    <* Angstrom.option ';' (char ';')
    <?> "gff2_attributes"

  let gff3_attribute_id = take_while1 (function
      | ',' | ';' | '=' | '\n' | ' ' -> false
      | _ -> true
    )

  let gff3_attribute_value =
    take_while (function
        | ',' | ';' | '\n' | '=' | '\t' -> false
        | _ -> true
      )
    >>| Uri.pct_decode

  let gff3_attribute_value_list =
    sep_by (char ',') gff3_attribute_value

  let gff3_attribute =
    gff3_attribute_id >>| Uri.pct_decode >>= fun id ->
    char '=' >>= fun _ ->
    gff3_attribute_value_list >>= fun xs ->
    return (id, xs)

  let gff3_attributes =
    sep_by attribute_separator gff3_attribute
    <?> "gff3_attributes"

  let tab = char '\t'

  let record version =
    let attributes = match version with
      | `gff2 -> gff2_attributes
      | `gff3 -> gff3_attributes
    in
    field "seqname" >>= fun seqname ->
    tab *>
    field "source" >>| option >>= fun source ->
    tab *>
    field "feature" >>| option >>= fun feature ->
    tab *>
    field "start_pos" >>= to_int 4 >>= fun start_pos ->
    tab *>
    field "stop_pos" >>= to_int 5 >>= fun stop_pos ->
    tab *>
    field "score" >>= maybe_convert (to_float 6) >>= fun score ->
    tab *>
    field "strand" >>= strand 7 >>= fun strand ->
    tab *>
    field "phase" >>= maybe_convert (to_int 8) >>= fun phase ->
    tab *>
    attributes >>| fun attributes ->
    `Record {
      Record.seqname ;
      source ;
      feature ;
      start_pos ;
      stop_pos ;
      score ;
      strand ;
      phase ;
      attributes ;
    }
  let record version = record version <?> "record"

  let comment =
    char '#' >>= fun _ ->
    take_while (Char.( <> ) '\n') >>| fun s ->
    `Comment s

  let space =
    skip_while (function
        | ' ' | '\t' -> true
        | _ -> false
      )

  let file version =
    let rec line_start acc lno =
      (end_of_input *> return acc)
      <|> (comment >>= fun c -> after_comment (c :: acc) lno )
      <|> (record version >>= fun r -> after_record (r :: acc) lno)
    and after_comment acc lno =
      (end_of_line >>= fun () -> line_start acc (lno + 1))
      <|> (end_of_input *> return acc)
    and after_record acc lno =
      (end_of_input *> return acc)
      <|> (space *> end_of_line >>= fun () -> line_start acc (lno + 1))
      <|> (space *> comment >>= fun c -> after_comment (c :: acc) lno)
    in
    line_start [] 1
    >>| List.rev

  let test p s v =
    match parse_string p s ~consume:All with
    | Ok x -> Poly.(x = v)
    | Error msg ->
      print_endline msg ;
      false

  let%test "Gff.gff2_attributes1" =
    test
      gff2_attributes
      {|gene_id "FBgn0031081"|}
      [ ("gene_id", ["FBgn0031081"]) ]

  let%test "Gff.gff2_attributes2" =
    test
      gff2_attributes
      {|gene_id "FBgn0031081"; gene_symbol "Nep3"; transcript_id "FBtr0070000"; transcript_symbol "Nep3-RA";|}
      [ ("gene_id", ["FBgn0031081"]) ; ("gene_symbol", ["Nep3"]) ;
        ("transcript_id", ["FBtr0070000"]) ; ("transcript_symbol", ["Nep3-RA"]) ]

  let%test "Gff.gff2_attributes3" =
    test
      gff2_attributes
      {|Transcript B0273.1; Note "Zn-Finger"|}
      [ "Transcript", ["B0273.1"] ; "Note", ["Zn-Finger"]]

  let%test "Gff.comment" =
    test comment "#comment" (`Comment "comment")

  type item = [ `Comment of string | `Record of Record.t ]
  [@@ deriving sexp]

  let expect_record version s =
    parse_string ~consume:All (record version) s
    |> [%sexp_of: ([`Record of Record.t], string) Result.t]
    |> Sexp.output_hum Stdio.stdout

  let expect version s =
    parse_string ~consume:All (file version) s
    |> [%sexp_of: (item list, string) Result.t]
    |> Sexp.output_hum Stdio.stdout

  let ex1 = {|IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger";|}
  let%expect_test "parse GFF2 record" =
    expect_record `gff2 ex1;
    [%expect {|
      (Ok
       (Record
        ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
         (stop_pos 5508917) (score ()) (strand Plus) (phase ())
         (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))) |}]

  let ex2 = {|X	FlyBase	gene	19961297	19969323	.	+	.	gene_id "FBgn0031081"; gene_symbol "Nep3";|}
  let%expect_test "parse GFF2 record 2" =
    expect_record `gff2 ex2;
    [%expect {|
      (Ok
       (Record
        ((seqname X) (source (FlyBase)) (feature (gene)) (start_pos 19961297)
         (stop_pos 19969323) (score ()) (strand Plus) (phase ())
         (attributes ((gene_id (FBgn0031081)) (gene_symbol (Nep3))))))) |}]

  let ex2 = {|IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger" # some comment
IV	curated	mRNA	5506800	5508917	.	+	.	Transcript B0273.1; Note "Zn-Finger"
X	FlyBase	gene	19961297	19969323	.	+	.	gene_id "FBgn0031081"; gene_symbol "Nep3";|}

  let%expect_test "parse GFF2 file" =
    expect `gff2 ex2;
    [%expect {|
      (Ok
       ((Record
         ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
          (stop_pos 5508917) (score ()) (strand Plus) (phase ())
          (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))
        (Comment " some comment")
        (Record
         ((seqname IV) (source (curated)) (feature (mRNA)) (start_pos 5506800)
          (stop_pos 5508917) (score ()) (strand Plus) (phase ())
          (attributes ((Transcript (B0273.1)) (Note (Zn-Finger))))))
        (Record
         ((seqname X) (source (FlyBase)) (feature (gene)) (start_pos 19961297)
          (stop_pos 19969323) (score ()) (strand Plus) (phase ())
          (attributes ((gene_id (FBgn0031081)) (gene_symbol (Nep3)))))))) |}]

  let%expect_test "parse ensembl gtf example" =
    expect `gff2 Handwritten_parser.ensembl_gtf_example ;
    [%expect {|
      (Ok
       ((Record
         ((seqname 1) (source (transcribed_unprocessed_pseudogene))
          (feature (gene)) (start_pos 11869) (stop_pos 14409) (score ())
          (strand Plus) (phase ())
          (attributes
           ((gene_id (ENSG00000223972)) (gene_name (DDX11L1))
            (gene_source (havana))
            (gene_biotype (transcribed_unprocessed_pseudogene))))))
        (Record
         ((seqname 1) (source (processed_transcript)) (feature (transcript))
          (start_pos 11869) (stop_pos 14409) (score ()) (strand Plus) (phase ())
          (attributes
           ((gene_id (ENSG00000223972)) (transcript_id (ENST00000456328))
            (gene_name (DDX11L1)) (gene_source (havana))
            (gene_biotype (transcribed_unprocessed_pseudogene))
            (transcript_name (DDX11L1-002)) (transcript_source (havana)))))))) |}]

  let%expect_test "parse ensembl gff example" =
    expect `gff3 Handwritten_parser.ensembl_gff_example ;
    [%expect {|
      (Ok
       ((Record
         ((seqname X) (source (Ensembl)) (feature (Repeat)) (start_pos 2419108)
          (stop_pos 2419128) (score (42)) (strand Not_stranded) (phase ())
          (attributes ((hid (trf)) (hstart (1)) (hend (21))))))
        (Record
         ((seqname X) (source (Ensembl)) (feature (Repeat)) (start_pos 2419108)
          (stop_pos 2419410) (score (2502)) (strand Minus) (phase ())
          (attributes ((hid (AluSx)) (hstart (1)) (hend (303))))))
        (Record
         ((seqname X) (source (Ensembl)) (feature (Repeat)) (start_pos 2419108)
          (stop_pos 2419128) (score (0)) (strand Not_stranded) (phase ())
          (attributes ((hid (dust)) (hstart (2419108)) (hend (2419128))))))
        (Record
         ((seqname X) (source (Ensembl)) (feature (Pred.trans.))
          (start_pos 2416676) (stop_pos 2418760) (score (450.19)) (strand Minus)
          (phase (2)) (attributes ((genscan (GENSCAN00000019335))))))
        (Record
         ((seqname X) (source (Ensembl)) (feature (Variation)) (start_pos 2413425)
          (stop_pos 2413425) (score ()) (strand Plus) (phase ()) (attributes ())))
        (Comment "")
        (Record
         ((seqname X) (source (Ensembl)) (feature (Variation)) (start_pos 2413805)
          (stop_pos 2413805) (score ()) (strand Plus) (phase ()) (attributes ())))
        (Comment ""))) |}]
end

let angstrom_parser = Angstrom_parser.file

module GFF2 = struct
  module Item = struct
    type t = item
    let parse l =
      Handwritten_parser.parse_item `gff2 l
      |> Rresult.R.failwith_error_msg

    let unparse = Handwritten_parser.unparse_item `gff2
  end
  include Line_oriented.Make(Item)
end

module GFF3 = struct
  module Item = struct
    type t = item
    let parse l =
      Handwritten_parser.parse_item `gff3 l
      |> Rresult.R.failwith_error_msg

    let unparse = Handwritten_parser.unparse_item `gff3
  end
  include Line_oriented.Make(Item)
end

module type S = sig
  include Line_oriented.S with type item := item

  module Item : sig
    type t = item
    val parse : Line.t -> [> `Comment of string | `Record of Record.t ]
    val unparse : item -> string
  end
end

module Annotation = struct
  type t = {
    transcript_id_label : string ;
    items : Record.t list String.Table.t ;
  }

  let of_items ?(gene_id_label = "gene_id") ?(transcript_id_label = "transcript_id") (items : item list) =
    let items =
      Stdlib.List.to_seq items
      |> Stdlib.Seq.filter_map (function
          | `Comment _ -> None
          | `Record r ->
            match List.Assoc.find r.Record.attributes gene_id_label ~equal:String.equal with
            | Some (id :: _) -> Some (id, r)
            | Some []
            | None -> None
        )
      |> Binning.relation
      |> Seq.map (fun (x, y) -> x, List.rev y)
      |> Stdlib.List.of_seq
      |> String.Table.of_alist_exn
    in
    { transcript_id_label ; items }

  let%test_unit "Annotation.of_items tail rec" =
    List.init 1_000_000 ~f:(fun _ -> `Comment "")
    |> of_items
    |> ignore

  let strand_of_entry items =
    let strands =
      List.map items ~f:(fun i -> i.Record.strand)
      |> List.dedup_and_sort ~compare:Poly.compare
    in
    match strands with
    | [] -> raise (Invalid_argument "strand_of_entry")
    | [ s ] -> Ok s
    | _ -> Or_error.error_string "more than one strand in entry"

  let exons_of_entry items =
    List.filter_map items ~f:(fun r ->
        match r.Record.feature with
        | Some "exon" -> Some r
        | _ -> None
      )

  let sort_by_attribute ~attr_label items =
    Stdlib.List.to_seq items
    |> Stdlib.Seq.filter_map (fun r ->
        match List.Assoc.find r.Record.attributes attr_label ~equal:String.equal with
          | Some (id :: _) -> Some (id, r)
          | Some []
          | None -> None
      )
    |> Binning.relation
    |> Stdlib.List.of_seq

  let gene_of_entry ~id ~transcript_id_label items =
    match exons_of_entry items with
    | [] -> Ok None
    | exons ->
      let open Or_error.Monad_infix in
      strand_of_entry exons |> Or_error.tag ~tag:id >>= function
      | `Unknown | `Not_stranded -> Or_error.error_string "no defined strand"
      | `Plus | `Minus as strand ->
        let transcripts =
          sort_by_attribute ~attr_label:transcript_id_label exons
          |> List.map ~f:(fun (s, items) -> s, List.map ~f:Record.loc items)
        in
        Gene.make ~strand ~id transcripts
        |> Or_error.map ~f:Option.some

  let genes annot =
    let r = String.Table.create () in
    let errors =
      Hashtbl.fold annot.items ~init:[] ~f:(fun ~key:id ~data:items errors ->
          match gene_of_entry ~id ~transcript_id_label:annot.transcript_id_label items with
          | Ok (Some g) ->
            Hashtbl.set r ~key:id ~data:g ;
            errors
          | Ok None -> errors
          | Error e -> (id, e) :: errors
        )
    in
    r, errors

  let utr5' annot =
    Hashtbl.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Record.feature with
            | Some "5UTR" -> Some r
            | _ -> None
          )
      )

  let utr3' annot =
    Hashtbl.filter_map annot.items ~f:(fun items ->
        List.find_map items ~f:(fun r ->
            match r.Record.feature with
            | Some "3UTR" -> Some r
            | _ -> None
          )
      )
end
