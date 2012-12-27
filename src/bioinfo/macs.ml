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
    type file = Row.t Guizmin_table.file
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
    type file = Row.t Guizmin_table.file
  end
  let run ?tagsize ?bandwidth ~genome ~pvalue ~control chIP =
    run14 ~genome ?tagsize ?bandwidth ~pvalue ~control chIP

  let peaks mo = select mo "macs_peaks.xls"
end

(* type peak = { *)
(*   loc : Location.t ; *)
(*   length : int ; *)
(*   summit : int ; *)
(*   tags : int ; *)
(*   pvalue : float ; *)
(*   fold : float ; *)
(*   fdr : float option ; *)
(* } *)

(* let peak_line_parser line = { *)
(*   loc = ( *)
(*     Location.make f.(0) (max 1 (int_of_string f.(1))) (int_of_string f.(2))  *)
(*     (\* shitty macs that may return negative coordinates !!! *\) *)
(*   ) ; *)
(*   length = int_of_string f.(3) ; *)
(*   summit = int_of_string f.(4) ;  *)
(*   tags = int_of_string f.(5) ; *)
(*   pvalue = float_of_string f.(6) ; *)
(*   fold = float_of_string f.(7) ; *)
(*   fdr =  *)
(*     if Array.length f = 9  *)
(*     then Some (float_of_string f.(8)) *)
(*     else None *)
(* } *)

(* let peak_parser fp =  *)
(*   Guizmin_table.parse ~header:true peak_line_parser fp *)
 
(* let peaks mo = select mo "macs_peaks.xls" *)
let bed mo = select mo "macs_peaks.bed"
