open Core_kernel
open Rresult
open Biocaml_unix

type index = Bai.bin Int.Table.t String.Table.t

type t = {
  bam : Bgzf.in_channel ;
  bam_header : Bam.Header.t ;
  index : index ;
}

let make_index bai bam_header : index =
  let foreach_reference_sequence name bai_reference_sequence =
    let r = Int.Table.create () in
    let bins = bai_reference_sequence.Bai.bins in
    Array.iter bins ~f:(fun b -> Int.Table.set r ~key:b.Bai.bin ~data:b) ;
    (name, r)
  in
  let reference_sequences =
    (Bam.Header.to_sam bam_header).ref_seqs
    |> List.map ~f:(fun (r : Sam.ref_seq) -> r.name)
    |> Array.of_list
  in
  Array.map2_exn reference_sequences bai.Bai.reference_sequences ~f:foreach_reference_sequence
  |> Array.to_list
  |> String.Table.of_alist_exn

let bgzf_open fn =
  try R.ok (Bgzf.open_in fn)
  with Sys_error _ ->
    R.error_msgf "Could not open %s" fn

let bam_read_header iz =
  Bam.read_header iz
  |> Result.map_error ~f:(fun _ -> R.msg "Could not parse BAM header")

let create ~bam ~bai =
  let open Result.Let_syntax in
  let%bind bam_ic = bgzf_open bam
  and bai = In_channel.with_file ~f:Bai.read bai in
  let%bind bam_header = bam_read_header bam_ic in
  let index = make_index bai bam_header in
  Ok { bam = bam_ic ; bam_header ; index }

exception Interrupt of string

let process_chunk iterator f ~(loc : GLoc.t) acc chunk =
  let rec loop acc =
    let current = Bgzf.virtual_offset iterator.bam in
    if Int64.(current >= chunk.Bai.chunk_end) then acc
    else
      match Bam.read_alignment iterator.bam with
      | None -> raise (Interrupt "Expected more alignments, truncated file?")
      | Some (Error e) -> raise (Interrupt (Core.Error.to_string_hum e))
      | Some (Ok al) ->
        match Bam.Alignment0.(pos al, tlen al) with
        | (Some pos, Some tlen) ->
          let pos = pos - 1 in (* pos is 1-based *)
          if pos + Int.abs tlen < loc.lo then loop acc
          else if pos > loc.hi then acc
          else (
            (* printf "%d %d\n" pos (pos + Int.abs tlen) ; *)
            loop (f acc iterator.bam_header al)
          )
        | _ -> loop (f acc iterator.bam_header al)
  in
  Bgzf.seek_in iterator.bam chunk.Bai.chunk_beg ;
  loop acc

let fold0 ~bam ~bai ~(loc : GLoc.t) ~init ~f =
  let open Result.Let_syntax in
  let%bind iterator = create ~bam ~bai in
  match String.Table.find iterator.index loc.chr with
  | None -> R.error_msgf "Unknown reference sequence %s" loc.chr
  | Some idx ->
    try R.ok @@
      Bai.reg2bins loc.lo loc.hi ~init ~f:(fun acc i ->
        match Int.Table.find idx i with
        | None -> acc
        | Some bin ->
          Array.fold bin.chunks ~init:acc ~f:(process_chunk iterator ~loc f)
        )
    with Interrupt msg -> Error (`Msg msg)
