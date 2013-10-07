open Core.Std
open CFStream
open Guizmin

type _ format =
| Sanger : [`sanger] format
| Solexa : [`solexa] format
| Phred64 : [`phred64] format

let sanger = Sanger
let solexa = Solexa
let phred64 = Phred64


type 'a file = 'a format Guizmin.file

type sanger = [`sanger] file

let map fq ~f ~label =
  f1
    "guizmin.bioinfo.fastq.map[r1]" [ Param.string "transformation" label ]
    fq
    (
      fun env (File fq) path ->
	let open Biocaml in
	In_channel.with_file fq ~f:(fun ic ->
	  Biocaml.Fastq.in_channel_to_item_stream_exn ic
          |> Stream.map ~f
	  |> Transform.to_stream_fun (Biocaml.Fastq.Transform.item_to_string ())
	  |>
	      fun xs ->
		Out_channel.with_file path ~f:(fun oc -> Stream.iter xs ~f:(output_string oc))
	)
    )

let item_quality_conversion f item =
  let open Biocaml.Fastq in
  { item with qualities = String.map item.qualities ~f }

let sanger_of_solexa_quality_conversion c =
  let open Biocaml in
  Solexa_score.of_ascii c
  |> Phred_score.of_solexa_score
  |> Phred_score.to_ascii_exn ~offset:`offset33

let sanger_of_solexa fq =
  map fq
    ~f:(item_quality_conversion sanger_of_solexa_quality_conversion)
    ~label:"sanger_of_solexa_quality_conversion"

let sanger_of_phred64_quality_conversion c =
  let open Biocaml in
  Phred_score.of_ascii_exn ~offset:`offset64 c
  |> Phred_score.to_ascii_exn ~offset:`offset33

let sanger_of_phred64 fq =
  map fq
    ~f:(item_quality_conversion sanger_of_phred64_quality_conversion)
    ~label:"sanger_of_phred64_quality_conversion"


let to_sanger (type s) (format : s format) (fq : s file) : sanger =
  match format with
  | Sanger -> fq
  | Solexa -> sanger_of_solexa fq
  | Phred64 -> sanger_of_phred64 fq

let nbreads fq =
  v1
    "guizmin.bioinfo.fastq.nbreads[r1]" []
    fq
    (
      fun env (File fq) ->
	In_channel.with_file fq ~f:(fun ic ->
	  Biocaml.Fastq.in_channel_to_item_stream_exn ic
          |> Stream.fold ~init:0 ~f:(fun accu _ -> succ accu)
	)
    )
