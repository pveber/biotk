open Core.Std
open CFStream
open Guizmin

type 'a format = 'a
let sanger = `sanger
let solexa = `solexa
let phred64 = `phred64


type 'a file = 'a format Guizmin.file

type sanger = [`sanger] file

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
