open GzmUtils
open GzmCore

type index

let index ?(packed = false) fasta =
  d1
    "guizmin.bioinfo.bowtie.index[r4]"
    [ Param.bool "packed" packed ]
    fasta
    (fun env (File fa) path ->
      env.bash [
	sp "mkdir %s" path ;
	sp "bowtie-build %s -f %s %s/index"
	  (if packed then "-a -p" else "") fa path ;
	sp "cd %s && cp `readlink -f %s` index.fa" path fa
      ])

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq.Phred64 -> "--phred64-quals"

let qual_param id v = Param.string id (qual_option v)

let align_with_maq_policy ?l ?e ?m ?fastq_format ~n index fastq_files =
  f2
    "guizmin.bioinfo.bowtie.align_with_maq_policy[r2]"
    Param.(
      [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ;
        opt qual_param "fastq_format" fastq_format]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      env.bash [
	<:sprint<bowtie -S -n $d:n$ \
                        $? l <- l${-l $d:l$} \
                        $? e <- e${-e $d:e$} \
                        $? m <- m${-m $d:m$} \
	                $? q <- fastq_format${qual_option q} \
                        -p $d:env.np$ \
	                $s:index$/index \
	                $!File f <- fastq_files ${$s:f$}{,} \
                        $s:path$ >>
      ])

let align ?m ?fastq_format ~v index fastq_files =
  f2
    "guizmin.bioinfo.bowtie.align[r2]"
    Param.(
      [ int "v" v ; opt int "m" m ;
        opt qual_param "fastq_format" fastq_format ]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      let cmd =
	<:sprint<bowtie -S -v $d:v$ $? m <- m${-m $d:m$} $? q <- fastq_format${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >>
      in
      env.bash [ cmd ])



















