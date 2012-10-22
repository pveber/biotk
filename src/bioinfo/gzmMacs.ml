open Guizmin

type output

let run14 ~genomesize ~tagsize ~bandwidth ~pvalue ?input chIP =
  match input with 
    | None -> 
      f2 
	("guizmin.bioinfo.gzmMacs.run14[r1]", 
	 [ int "tagsize" tagsize ; int "bandwidth" bandwidth ;
	   float "pvalue" pvalue ])
	(fun env genomesize (File chIP) path ->
	  bash ~env [
	    <:sprint<mkdir $s:path$>> ;
	    <:sprint<macs14 --name=$s:path$/macs --gsize=$d:70 * genomesize / 100$ \
                     --tsize=$d:tagsize$ --bw=$d:bandwidth$ --pvalue=$g:pvalue$ \
                     -t $s:chIP$>> ;
	  ])
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

:
