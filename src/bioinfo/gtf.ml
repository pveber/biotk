open Core.Std
open Guizmin

type format
type file = format Guizmin.file
type file_path = format Guizmin.file_path

let stream_of_channel ?(buffer_size = 4096) ~t ic =
  let fd = Unix.descr_of_in_channel ic in
  let buf = String.make buffer_size ' ' in
  let rec loop () =
    match Biocaml_transform.next t with
    | `output o -> Some o
    | `end_of_stream -> None
    | `not_ready ->
      (
        match Unix.read fd ~buf ~pos:0 ~len:buffer_size with 
        | 0 -> Biocaml_transform.stop t
        | _ -> Biocaml_transform.feed t buf
      ) ;
      loop ()
  in
  Biocaml_stream.from (fun _ -> loop ())

let with_file (File gff) ~f =
  In_channel.with_file gff ~f:(fun ic ->
    Biocaml_gff.Transform.string_to_item ~tags:[] ()
    |! (fun t -> stream_of_channel t ic)
    |! Biocaml_stream.result_to_exn ~error_to_exn:(fun _ -> assert false)
    |! f
  )



















