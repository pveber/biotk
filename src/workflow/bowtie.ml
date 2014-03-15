open Types

type index = [`bowtie_index] directory

let package = Bistro_workflow.make <:script<

URL=http://sourceforge.net/projects/bowtie-bio/files/bowtie/1.0.0/bowtie-1.0.0-src.zip/download
ARCHIVE=`basename ${URL%\/download}`
PACKAGE=${ARCHIVE%\.tar.gz}

wget -O ${ARCHIVE} ${URL}
unzip ${ARCHIVE}
cd ${ARCHIVE%\-src.zip}
make
mkdir -p #DEST/bin
cp bowtie bowtie-build bowtie-inspect #DEST/bin

>>

let bowtie_build ?packed ?color fa = Bistro_workflow.make <:script<
  export PATH=$w:package$/bin:$PATH
  mkdir #DEST
  bowtie-build \
    #? p <- packed #[if p then "-a -p" else ""] \
    #? c <- color #[ if c then "--color" else ""] \
    -f #w:fa# #DEST/index
  (cd #DEST && cp `readlink -f #w:fa#` index.fa)

>>

let qual_option (type s) x = match (x : s Fastq.format) with
  | Fastq.Solexa  -> "--solexa-quals"
  | Fastq.Sanger -> "--phred33-quals"
  | Fastq.Phred64 -> "--phred64-quals"

let bowtie ?l ?e ?m ?fastq_format ?n ?v index fastq_files = Bistro_workflow.make <:script<
  export PATH=$w:package$/bin:$PATH
  bowtie -S \
    #? n <- n #[-n #i:n#] \
    #? l <- l #[-n #i:l#] \
    #? e <- e #[-n #i:e#] \
    #? m <- m #[-n #i:m#] \
    #? v <- v #[-n #i:v#] \
    #? q <- fastq_format #[qual_option q] \
    -p #i:assert false# \
    #w:index#/index \
    #! f <- fastq_files #[#w:f#][,] \
    #DEST
>>

(* let align_with_maq_policy ?l ?e ?m ?fastq_format ~n index fastq_files = *)
(*   f2 *)
(*     "guizmin.bioinfo.bowtie.align_with_maq_policy[r2]" *)
(*     Param.( *)
(*       [ int "n" n ; opt int "l" l ; opt int "e" e ; opt int "m" m ; *)
(*         opt qual_param "fastq_format" fastq_format] *)
(*     ) *)
(*     index (merge fastq_files) *)
(*     (fun env (Dir index) fastq_files path -> *)
(*       env.bash [ *)
(* 	<:sprint<bowtie -S -n $d:n$ \ *)
(*                         $? l <- l${-l $d:l$} \ *)
(*                         $? e <- e${-e $d:e$} \ *)
(*                         $? m <- m${-m $d:m$} \ *)
(* 	                $? q <- fastq_format${qual_option q} \ *)
(*                         -p $d:env.np$ \ *)
(* 	                $s:index$/index \ *)
(* 	                $!File f <- fastq_files ${$s:f$}{,} \ *)
(*                         $s:path$ >> *)
(*       ]) *)

(* let align ?m ?fastq_format ~v index fastq_files = *)
(*   f2 *)
(*     "guizmin.bioinfo.bowtie.align[r2]" *)
(*     Param.( *)
(*       [ int "v" v ; opt int "m" m ; *)
(*         opt qual_param "fastq_format" fastq_format ] *)
(*     ) *)
(*     index (merge fastq_files) *)
(*     (fun env (Dir index) fastq_files path -> *)
(*       let cmd = *)
(* 	<:sprint<bowtie -S -v $d:v$ $? m <- m${-m $d:m$} $? q <- fastq_format${qual_option q} -p $d:env.np$ $s:index$/index $!File f <- fastq_files ${$s:f$}{,} $s:path$ >> *)
(*       in *)
(*       env.bash [ cmd ]) *)



















