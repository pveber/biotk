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
open Guizmin
open GzmUtils

let indexed_bam_of_sam sam =
  d1
    "guizmin.bioinfo.samtools.indexed_bam_of_sam[r2]"
    []
    sam
    (fun env (File sam) path ->
       env.sh "mkdir %s" path ;
       env.sh "samtools view -S -b -o %s/temp.bam %s" path sam ;
       env.sh "samtools sort %s/temp.bam %s/reads" path path ;
       env.sh "samtools index %s/reads.bam" path ;
       env.sh "rm %s/temp.bam" path 
    )

let indexed_bam_of_bam bam =
  d1
    "guizmin.bioinfo.samtools.indexed_bam_of_bam[r2]"
    []
    bam
    (fun env (File bam) path ->
      env.sh "mkdir %s" path ;
      env.sh "samtools sort %s %s/reads" bam path ;
      env.sh "samtools index %s/reads.bam" path)

let bam_of_indexed_bam ibam = select ibam "reads.bam"

let bam_of_sam sam = bam_of_indexed_bam (indexed_bam_of_sam sam)  

let sam_of_bam bam =
  f1
    "guizmin.bioinfo.samtools.sam_of_bam[r2]"
    []
    bam
    (fun env (File bam) path -> sh "samtools view -o %s %s" path bam)

let rmdup ?(single_end_mode = false) bam =
  f1
    "guizmin.bioinfo.samtools.rmdup[r1]"
    [ Param.bool "single_end_mode" single_end_mode ]
    bam
    (fun env (File bam) path ->
       env.sh "mkdir -p %s_tmp" path ;
       env.sh "samtools sort %s %s_tmp/reads" bam path ;
       env.sh "%s" <:sprint<samtools rmdup $? single_end_mode ${-s}{} $s:path$_tmp/reads.bam $s:path$>> ;
       env.sh "rm -rf %s_tmp" path)
