open GzmUtils
open GzmCore

type index

let index ?(packed = false) fasta = 
  d1
    "guizmin.bioinfo.bowtie.index[r1]"
    [ Param.bool "packed" packed ]
    fasta
    (fun env (File fa) path ->
      env.bash [
	sp "mkdir %s" path ;
	sp "bowtie-build %s -f %s %s/index" 
	  (if packed then "-a -p" else "") fa path ;
	sp "ln -s %s index.fa" fa
      ])

let qual_option = function
  | `solexa  -> "--solexa-quals"
  | `phred33 -> "--phred33-quals"
  | `phred64 -> "--phred64-quals"

let qual_param id v = Param.string id (qual_option v)

let align_with_maq_policy ?l ?e ?m ?qual_kind ~n index fastq_files =
  f2
    "guizmin.bioinfo.bowtie.align_with_maq_policy[r1]"
    Param.(
      [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ;
        opt qual_param "qual_kind" qual_kind]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      env.bash [
	<:sprint<bowtie -n $d:n$ \
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
    "guizmin.bioinfo.bowtie.align[r1]"
    Param.(
      [ int "v" v ; opt int "m" m ;
        opt qual_param "qual_kind" qual_kind ]
    )
    index (merge fastq_files)
    (fun env (Dir index) fastq_files path ->
      let cmd = 
	<:sprint<bowtie -v $d:v$ $? m <- m${-m $d:m$} $? q <- qual_kind${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >>
      in
      print_endline cmd ;
      env.bash [ cmd ])



















