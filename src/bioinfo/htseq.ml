open Guizmin

let count ?(feature = "exon") sam gtf = 
  f2
    ("guizmin.bioinfo.htseq.count[r1]", [ string "feature" feature ])
    (fun (* FIXME ! env *) (File sam) (File gtf) path ->
	GzmUtils.bash (* FIXME ~env *) [
	  <:sprint<htseq-count -t $s:feature$ $s:sam$ $s:gtf$ > $s:path$>>
	])
    sam gtf
