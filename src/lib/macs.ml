open CFStream
open Core.Std

let read_xls f ic =
  Biocaml.Lines.of_channel ic
  |> Stream.map ~f:(fun x -> (x : Biocaml.Line.t :> string))
  |> Stream.filter ~f:(fun x -> not (x = "" || x.[0] = '#'))
  |> Stream.skip ~n:1
  |> Stream.map ~f:(String.split ~on:'\t')
  |> Stream.map ~f:Array.of_list
  |> Stream.map ~f

module No_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      len "length" : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
    }
  end

  let read_xls = read_xls Peak.Row.of_array
end

module With_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      len "length" : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
      fdr : float ;
    }
  end

  let read_xls = read_xls Peak.Row.of_array
end
