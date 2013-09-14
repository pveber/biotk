open Core.Std
open CFStream
open Stream.Infix
open Biocaml
open Guizmin
open MBSchema

type 'a output

let effective_genome_size = function
| `hg18 | `hg19 -> 2700000000
| `mm8 | `mm9 -> 1890000000
| `sacCer2 -> assert false

let run14 ~genome ?tagsize ?bandwidth ~pvalue ?control chIP =
  let gsize = effective_genome_size genome in
  match control with
    | None ->
      d1
        "guizmin.bioinfo.gzmMacs.run14[r2]"
        Param.([
          int "gsize" gsize ;
          opt int "tagsize" tagsize ;
          opt int "bandwidth" bandwidth ;
          float "pvalue" pvalue
        ])
        chIP
	(fun env (File chIP) path ->
	  env.bash [
	    <:sprint<mkdir $s:path$>> ;
	    <:sprint<macs14 --name=$s:path$/macs --gsize=$d:gsize$ \
                     $? ts <- tagsize${--tsize=$d:ts$} \
                     $? bw <- bandwidth${--bw=$d:bw$} \
                     --pvalue=$g:pvalue$ \
                     -t $s:chIP$>> ;
	  ])
    | Some control ->
      d2
	"guizmin.bioinfo.gzmMacs.run14[r2]"
	Param.([
          int "gsize" gsize ;
          opt int "tagsize" tagsize ;
          opt int "bandwidth" bandwidth ;
          float "pvalue" pvalue
        ])
	chIP control
	(fun env (File chIP) (File control) path ->
	  env.bash [
	    <:sprint<mkdir $s:path$>> ;
	    <:sprint<macs14 --name=$s:path$/macs --gsize=$d:gsize$ \
                     $? ts <- tagsize${--tsize=$d:ts$} \
                     $? bw <- bandwidth${--bw=$d:bw$} \
                     --pvalue=$g:pvalue$ \
                     -t $s:chIP$ -c $s:control$>> ;
	  ])

module Wo_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : int ;
      chromEnd : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
    }
    include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.Some_header)
  end
  let run ?tagsize ?bandwidth ~genome ~pvalue chIP =
    run14 ~genome ?tagsize ?bandwidth ~pvalue chIP

  let peaks mo = select mo "macs_peaks.xls"
end

module Natural = struct
  include Core.Std.Int
end

module With_control = struct
  module Peak = struct
    type tabular t = {
      chrom : string ;
      chromStart : Natural ;
      chromEnd : int ;
      summit : int ;
      tags : int ;
      pvalue : float ;
      fold : float ;
      fdr : float ;
    }
    include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.Sharp_comment)(Guizmin_table.Some_header)
  end

  let run ?tagsize ?bandwidth ~genome ~pvalue ~control chIP =
    run14 ~genome ?tagsize ?bandwidth ~pvalue ~control chIP

  let peaks mo = select mo "macs_peaks.xls"
end

let bed mo = select mo "macs_peaks.bed"

(** parses a line of a peak file *)
let parse_peak_line line = Wo_control.Peak.(
  String.split (line : Biocaml_lines.item :> string) ~on:'\t'
  |! Array.of_list
  |! Row.of_array
  |! Obj.of_row
)

let best_peaks ~n peaks =
  f1
    "guizmin.bioinfo.macs.best_peaks[r1]" Param.([int "n" n])
    peaks
    (
      fun env (File peaks) path ->

        let peaks = In_channel.with_file peaks ~f:(fun ic ->
          Biocaml_lines.of_channel ic
          |> Stream.filter ~f:(fun l -> let l = (l : Biocaml_lines.item :> string) in l <> "" && l.[0] <> '#')
          |> Stream.skip ~n:1
          |> Stream.map ~f:(fun l -> l, parse_peak_line l)
          |> Stream.to_list
        )
        in
        let sorted_peaks = List.sort ~cmp:(fun x y -> Float.compare (snd y)#pvalue (snd x)#pvalue) peaks in
        Stream.of_list sorted_peaks
        /@ fst
        |> Stream.take ~n
        |> fun xs -> Out_channel.with_file path ~f:(Biocaml_lines.to_channel xs)
    )


















