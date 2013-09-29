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

type supported_quality_encoding = [ `solexa | `sanger | `phred64 ]
type 'a quality_encoding = 'a Fastq.format constraint 'a = [< supported_quality_encoding ]

let qual_option x = match (x : 'a quality_encoding :> supported_quality_encoding) with
  | `solexa  -> "--solexa-quals"
  | `sanger -> "--phred33-quals"
  | `phred64 -> "--phred64-quals"

let qual_param id v = Param.string id (qual_option v)

let align_with_maq_policy ?l ?e ?m ?qual_kind ~n index fastq_files =
  f2
    "guizmin.bioinfo.bowtie.align_with_maq_policy[r2]"
    Param.(
      [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ;
        opt qual_param "qual_kind" qual_kind]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      env.bash [
	<:sprint<bowtie -S -n $d:n$ \
                        $? l <- l${-l $d:l$} \
                        $? e <- e${-e $d:e$} \
                        $? m <- m${-m $d:m$} \
	                $? q <- qual_kind${qual_option q} \
                        -p $d:env.np$ \
	                $s:index$/index \
	                $!File f <- fastq_files ${$s:f$}{,} \
                        $s:path$ >>
      ])

(* let align2 =  *)
(*   file pipeline "guizmin.bioinfo.bowtie.align[r1]"  *)
(*     ?(n:int) ?(l:int) ?(e:int) ?(m:int) ?(p:int) *)
(*     uses index fastq_files* -> ( *)
(*       let qual_kind = None in *)
(*       let File index = index in *)
(*       _env.bash [ *)
(* 	<:sprint<bowtie $? n <- n${-n $d:n$} \ *)
(*                         $? l <- l${-l $d:l$} \ *)
(*                         $? e <- e${-e $d:e$} \ *)
(*                         $? m <- m${-m $d:m$} \ *)
(*                         $? p <- p${-p $d:p$} \ *)
(* 	                $? q <- qual_kind${qual_option q} \ *)
(* 	                $s:index$ \  *)
(* 	                $!File f <- fastq_files ${$s:f$}{,} \ *)
(*                         $s:_path$ >> *)
(*       ] *)
(*     ) *)

let align ?m ?qual_kind ~v index fastq_files =
  f2
    "guizmin.bioinfo.bowtie.align[r2]"
    Param.(
      [ int "v" v ; opt int "m" m ;
        opt qual_param "qual_kind" qual_kind ]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      let cmd =
	<:sprint<bowtie -S -v $d:v$ $? m <- m${-m $d:m$} $? q <- qual_kind${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >>
      in
      env.bash [ cmd ])



















