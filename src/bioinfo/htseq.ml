open Guizmin

module Output = struct
  type tabular data = {
    id : string ;
    count : int
  }
  type format
  type file = format Guizmin_table.file
end

let count ?(feature = "exon") sam gtf = 
  f2
    Param.(
      "guizmin.bioinfo.htseq.count[r1]", 
      [ string "feature" feature ]
    )
    (fun env (File sam) (File gtf) path ->
	env.bash [
	  <:sprint<htseq-count -t $s:feature$ $s:sam$ $s:gtf$ > $s:path$>>
	])
    sam gtf











