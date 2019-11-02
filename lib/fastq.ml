open Core_kernel
open Biocaml_unix

type item = Fastq.item

exception Abort of Error.t

let fold_file fn ~init ~f =
  In_channel.with_file fn ~f:(fun ic ->
      try
        Fastq.read ic
        |> CFStream.Stream.fold ~init ~f:(fun acc item_or_error ->
            match item_or_error with
            | Ok it -> f acc it
            | Error e -> raise (Abort e)
          )
        |> Or_error.return
      with Abort e -> Error e
    )
