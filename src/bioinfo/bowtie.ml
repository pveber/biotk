open GzmUtils
open GzmCore

type index

let index ?(packed = false) fasta = 
  let full_index = 
    d1
      ("guizmin.bioinfo.bowtie.index[r1]", [ bool "packed" packed ])
      (fun (File fa) path ->
	bash [
	  sp "mkdir %s" path ;
	  sp "bowtie-build %s -f %s %s/index" 
	    (if packed then "-a -p" else "") fa path ;
	  sp "ln -s %s index.fa" fa
	])
      fasta
  in
  select full_index "index"

let qual_option = function
  | `solexa  -> "--solexa-quals"
  | `phred33 -> "--phred33-quals"
  | `phred64 -> "--phred64-quals"

let qual_param id v = string id (qual_option v)

let align ?n ?l ?e ?m ?p ?qual_kind index fastq_files =
  f2
    ("guizmin.bioinfo.bowtie.align[r1]", 
     [] +? opt int "n" n 
        +? opt int "l" l 
        +? opt int "e" e 
        +? opt int "m" m 
        +? opt int "p" p
        +? opt qual_param "qual_kind" qual_kind)
    (fun (File index) fastq_files path ->
      bash ~debug:true [
	<:sprint<bowtie $? n <- n${-n $d:n$} \
                        $? l <- l${-l $d:l$} \
                        $? e <- e${-e $d:e$} \
                        $? m <- m${-m $d:m$} \
                        $? p <- p${-p $d:p$} \
	                $? q <- qual_kind${qual_option q} \
	                $s:index$ \ 
	                $!File f <- fastq_files ${$s:f$}{,} \
                        $s:path$ >>
      ])
    index (merge fastq_files)

let align2 = 
  file pipeline "guizmin.bioinfo.bowtie.align[r1]" 
    ?(n:int) ?(l:int) ?(e:int) ?(m:int) ?(p:int)
    uses index fastq_files* -> (
      let qual_kind = None in
      let File index = index in
      bash ~debug:true [
	<:sprint<bowtie $? n <- n${-n $d:n$} \
                        $? l <- l${-l $d:l$} \
                        $? e <- e${-e $d:e$} \
                        $? m <- m${-m $d:m$} \
                        $? p <- p${-p $d:p$} \
	                $? q <- qual_kind${qual_option q} \
	                $s:index$ \ 
	                $!File f <- fastq_files ${$s:f$}{,} \
                        $s:_path$ >>
      ]
    )
