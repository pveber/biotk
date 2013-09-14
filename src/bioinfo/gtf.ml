open Core.Std
open CFStream
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
  Stream.from (fun _ -> loop ())

open Sexplib.Std

type parse_error = [ `cannot_parse_float of Biocaml_pos.t * string
                   | `cannot_parse_int of Biocaml_pos.t * string
                   | `cannot_parse_strand of Biocaml_pos.t * string
                   | `cannot_parse_string of Biocaml_pos.t * string
                   | `empty_line of Biocaml_pos.t
                   | `incomplete_input of Biocaml_pos.t * string list * string option
                   | `wrong_attributes of Biocaml_pos.t * string
                   | `wrong_row of Biocaml_pos.t * string
                   | `wrong_url_escaping of Biocaml_pos.t * string ]
with sexp

let with_file ?(tags = Biocaml_gff.Tags.({ default with allow_empty_lines = true })) (File gff) ~f =
  In_channel.with_file gff ~f:(fun ic ->
    Stream.strings_of_channel ic
    |! Biocaml_transform.to_stream_fun (Biocaml_gff.Transform.string_to_item tags ())
    |! Stream.result_to_exn ~error_to_exn:(fun e -> Failure (Sexp.to_string_hum (sexp_of_parse_error e)))
    |! f
  )



















