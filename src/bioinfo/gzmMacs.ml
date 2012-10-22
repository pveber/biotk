open Guizmin
open GzmMBSchema

type output

let run14 ~genomesize ~tagsize ~bandwidth ~pvalue ?input chIP =
  match input with 
    | None -> 
      d2 
	("guizmin.bioinfo.gzmMacs.run14[r1]", 
	 [ int "tagsize" tagsize ; int "bandwidth" bandwidth ;
	   float "pvalue" pvalue ])
	(fun (* FIXME env *) genomesize (File chIP) path ->
	  GzmUtils.bash (* FIXME ~env *) [
	    <:sprint<mkdir $s:path$>> ;
	    <:sprint<macs14 --name=$s:path$/macs --gsize=$d:70 * genomesize / 100$ \
                     --tsize=$d:tagsize$ --bw=$d:bandwidth$ --pvalue=$g:pvalue$ \
                     -t $s:chIP$>> ;
	  ])
	genomesize chIP
    | Some input ->
      assert false
	    
  (* (object  *)
  (*    method id =  *)
  (*      "Macs.run14[r2,tagsize=$d:tagsize$,bandwidth=$d:bandwidth$,pvalue=$g:pvalue$]"s  *)
  (*    method deps =  *)
  (*      genomesize#abstract ::  *)
  (*      (match input with  *)
  (*           None -> [] *)
  (*         | Some i -> [] ++ i) ++ chIP *)
  (*    method build path = *)
  (*      let base_cmd =  *)
  (*        "macs14 --name=$s:path$/macs --format=SAM --gsize=$d:70 * genomesize#value / 100$ \ *)
  (*                    --tsize=$d:tagsize$ --bw=$d:bandwidth$ --pvalue=$g:pvalue$ \ *)
  (*                    -t $s:chIP#path$"s in *)
  (*      let cmd = match input with  *)
  (*          None -> base_cmd *)
  (*        | Some input -> "$s:base_cmd$ -c $s:input#path$"s  *)
  (*      in *)
  (*        mkdir path ; shell cmd *)
  (*    method ty = `directory (`macs_output (match input with None -> `without_fdr | Some _ -> `with_fdr)) *)
  (*  end) *)

type peak = {
  loc : Location.t ;
  length : int ;
  summit : int ;
  tags : int ;
  pvalue : float ;
  fold : float ;
  fdr : float option ;
}

let peak_line_parser f = {
  loc = (
    Location.make f.(0) (max 1 (int_of_string f.(1))) (int_of_string f.(2)) 
    (* shitty macs that may return negative coordinates !!! *)
  ) ;
  length = int_of_string f.(3) ;
  summit = int_of_string f.(4) ; 
  tags = int_of_string f.(5) ;
  pvalue = float_of_string f.(6) ;
  fold = float_of_string f.(7) ;
  fdr = 
    if Array.length f = 9 
    then Some (float_of_string f.(8))
    else None
}

let peak_parser fp = 
  Guizmin_table.parse ~header:true peak_line_parser fp

let peaks mo = select mo "macs_peaks.xls"
let bed mo = select mo "macs_peaks.bed"
