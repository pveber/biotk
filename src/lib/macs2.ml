open Core.Std
open Biocaml_ez
open CFStream

module Narrow_peaks = struct
  type item = {
    loc : Seq_range.t ;
  }

  let of_line = function
    | [ chr ; st ; ed ] ->
      { loc = Seq_range.make chr (Int.of_string st) (Int.of_string ed) ; }
    | _ -> failwith "incorrect number of arguments in macs2 narrow peak file"

  let read ic =
    Lines.read ic
    |> Stream.map ~f:(Line.split ~on:'\t')
    |> Stream.map ~f:of_line
end
