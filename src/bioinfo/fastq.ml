open Core.Std
open CFStream
open Guizmin

type 'a format = 'a
let sanger = `sanger
let solexa = `solexa
let phred64 = `phred64


type 'a file = 'a format Guizmin.file

type sanger = [`sanger] file

let sanger_of_solexa fq =
  f1
    "guizmin.bioinfo.fastq.sanger_of_solexa[r1]" []
    fq
    (
      fun env (File fq) path ->
	let open Biocaml in
	let char_conversion c =
	  Solexa_score.of_ascii c
	  |> Phred_score.of_solexa_score
	  |> Phred_score.to_ascii_exn ~offset:`offset33
	in
	In_channel.with_file fq ~f:(fun ic ->
	  Biocaml.Fastq.in_channel_to_item_stream_exn ic
          |> Stream.map ~f:(fun item ->
	    { item with Biocaml.Fastq.qualities = String.map item.Biocaml.Fastq.qualities ~f:char_conversion }
	  )
	  |> Transform.to_stream_fun (Biocaml.Fastq.Transform.item_to_string ())
	  |>
	      fun xs ->
		Out_channel.with_file path ~f:(fun oc -> Stream.iter xs ~f:(output_string oc))
	)
    )

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
