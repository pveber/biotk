open Guizmin

module Output = struct
  type tabular data = {
    id : string ;
    count : int
  }
  include Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.No_comment_nor_header)
end

let count ?(feature = "exon") sam gtf = 
  f2
    "guizmin.bioinfo.htseq.count[r1]"      
    [ Param.string "feature" feature ]
    sam gtf
    (fun env (File sam) (File gtf) path ->
	env.bash [
	  <:sprint<htseq-count -t $s:feature$ $s:sam$ $s:gtf$ > $s:path$>>
	])











