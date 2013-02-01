(* This file is part of guizmin.

    guizmin is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    guizmin is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with guizmin.  If not, see <http://www.gnu.org/licenses/>.
*)
open Core.Std
open Guizmin

type output

let version =
  lazy (
    GzmUtils.shout "tophat --version"
    |! fun x -> Scanf.sscanf x "%s %s" (fun _ y -> y)
  )

let bowtie1_present = 
  lazy (
    Sys.command "which bowtie > /dev/null" = 0
  )

let bowtie2_present = 
  lazy (
    Sys.command "which bowtie2 > /dev/null" = 0
  )

let run index fastq_files =
  d2
    "guizmin.bioinfo.tophat.run"
    Param.([
      string "version" (Lazy.force version) ;
      bool "bowtie1_present" (Lazy.force bowtie1_present) ;
      bool "bowtie2_present" (Lazy.force bowtie2_present) ;
    ])
    index (merge fastq_files)
    (
      fun env (Dir index) fastq_files path ->
        env.sh 
          "tophat -p %d -o %s %s/index %s"
          env.np path index (String.concat ~sep:"," (List.map fastq_files ~f:(function File f -> f)))
    )
let aligned_reads dir = select dir "accepted_hits.bam"
let junctions dir = select dir (assert false)

(*

let aligned_reads output = 
  in_directory output "accepted_hits.bam" `BAM

let junctions output = 
  let original = in_directory output "junctions.bed" `bed_ucsc_track in F.make
    (object
       method id = "Tophat.junctions[r1]"
       method build path =
	 shell ("gawk '! /^track/ { printf \"%s\\t%s\\t%s\\t%s\\t%d\\t%s\\t%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n\", $1, $2, $3, $5, $5, $6, $7, $8, $9, $10, $11, $12}' " ^
		  original#path ^ " > " ^ path)
       method deps = [] ++ original
       method ty = `bed_ucsc_track
     end)


*)
