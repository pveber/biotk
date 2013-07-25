open Core.Std
open Guizmin

type meme_chip_output

(* for some reason meme's -p option does not seem to work... *)
let meme_chip ?meme_nmotifs ?meme_minw ?meme_maxw fa : meme_chip_output dir = 
  d1
    "guizmin.bioinfo.meme.meme_chip[r1]"
    Param.([ opt int "meme-nmotifs" meme_nmotifs ;
             opt int "meme-minw" meme_minw ;
             opt int "meme-maxw" meme_maxw ;
           ])
    fa
    (fun env (File fa) path ->
      env.bash [
        <:sprint<meme-chip $? n <- meme_nmotifs${-meme-nmotifs $d:n$} $? n <- meme_minw${-meme-minw $d:n$} $? n <- meme_maxw${-meme-maxw $d:n$} -oc $s:path$ $s:fa$>>
      ])

let meme_chip_meme_motifs (Dir output) =
  In_channel.with_file 
    (output ^ "/meme_out/meme.xml")
    ~f:Meme.(fun ic ->
      let xml = xml_of_channel ic in
      List.map
        (Option.value_exn xml).motifs
        (fun m -> m.motif_id, pwm_matrix_count_of_motif m))

(*
 sequence file must be specified.

Usage:
    meme-chip [options] <sequences>

     Options:
      -o                <dir>   : output to the specified directory, failing if the directory exists
      -oc               <dir>   : output to the specified directory, overwriting if the directory exists
      -index-name       <name>  : name of html index file; default: index.html
      -db               <path>  : target database for use by TOMTOM and AME, if not present then TOMTOM and AME are not run
      -bfile            <path>  : background file
      -nmeme            <num>   : limit of sequences to pass to MEME
      -ccut             <num>   : maximum size of a sequence before it is cut down 
                                  to a centered section; a value of 0 indicates the
                                  sequences should not be cut down; default: 100
      -desc             <text>  : description of the job
      -fdesc            <file>  : file containing plain text description of the job
      -run-mast                 : run MAST - motif alignment & search tool
      -run-ama                  : run AMA - Average motif affinity. 
      -noecho                   : don't echo the commands run
      -tar                      : create a tar.gz file of the outputs
      -help                     : display this help message

     MEME Specific Options:
      -meme-mod [oops|zoops|anr]: sites used in a single sequence
      -meme-minw        <num>   : minimum motif width
      -meme-maxw        <num>   : maximum motif width
      -meme-nmotifs     <num>   : maximum number of motifs to find
      -meme-minsites    <num>   : minimum number of sites per motif
      -meme-maxsites    <num>   : maximum number of sites per motif
      -meme-time        <secs>  : maximum time to run MEME in seconds
      -meme-p           <np>    : use parallel version with <np> processors
      -meme-norevcomp           : search given strand only
      -meme-pal                 : look for palindromes only

*)

let meme2images ?rc meme_motif =
  d1
    "guizmin.bioinfo.meme_suite.meme2png[r1]"
    [ Param.bool "rc" (rc = Some ()) ]
    meme_motif
    (fun env (File meme_motif) path ->
      let cmd = <:sprint<meme2images -eps -png $? () <- rc${-rc} $s:meme_motif$ $s:path$>> in
      env.bash [cmd])
