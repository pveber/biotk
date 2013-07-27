open GzmCore

type bigWig
type wig

type genome = [ `mm8 | `mm9 | `hg18 | `hg19 | `sacCer2 ]

val string_of_genome : genome -> string
val chromosome_sequences : genome -> [`ucsc_chromosome_sequences] dir
val genome_sequence : [< genome] -> Fasta.file
val genome_2bit_sequence : genome -> [`ucsc_2bit] file

val wg_encode_crg_mappability_36  : [`mm9 | `hg18 | `hg19] -> bigWig file
val wg_encode_crg_mappability_40  : [`mm9 | `hg18 | `hg19] -> bigWig file
val wg_encode_crg_mappability_50  : [`mm9 | `hg18 | `hg19] -> bigWig file
val wg_encode_crg_mappability_75  : [`mm9 | `hg18 | `hg19] -> bigWig file
val wg_encode_crg_mappability_100 : [`mm9 | `hg18 | `hg19] -> bigWig file

val fasta_of_bed : genome -> 'a Bed.named_file -> Fasta.file
val fetch_sequences : [`ucsc_2bit] file_path -> MBSchema.Location.t list -> string list

val wig_of_bigWig : bigWig file -> wig file

module Chrom_info : sig
  type tabular data = {
    chrom : string ;
    chrom_length : int
  }
  include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty
end
val chrom_info : genome -> Chrom_info.file
val bedClip : Chrom_info.file -> 'a Bed.file -> 'a Bed.file

module Lift_over : sig
  open MBSchema

  type chain_file = [`lift_over_chain] file
  val chain_file : org_from:genome -> org_to:genome -> chain_file

  (** [conversion fp xs] returns a pair of location lists, mapped and
      unmapped locations. *)
  val conversion : 
    [`lift_over_chain] file_path -> 
    Location.t Stream.t ->
    Location.t list * Location.t list

  (** liftOver preserves {b more or less} the input BED: columns are
      conserved but fields may be changed (floats truncated to integers) *)
  val bed_conversion : org_from:genome -> org_to:genome -> 'a Bed.file -> [`ucsc_lift_over of 'a] dir
  val mapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file
  val unmapped : [`ucsc_lift_over of 'a] dir -> 'a Bed.file
end


module CustomTrack : sig
  type option = [ 
    `track_type of string
  | `name of string 
  | `description of string
  | `visibility of [ `hide | `full | `dense ]
  | `color of int * int * int
  | `altColor of int * int * int
  | `priority of int
  | `autoScale of bool
  | `gridDefault of bool
  | `maxHeightPixels of int * int * int
  | `graphType of [ `bar | `points ]
  | `viewLimits of [ `lower | `upper ]
  | `yLineMark of float 
  | `yLineOnOff of bool
  | `windowingFunction of [ `maximum | `mean | `minimum ]
  | `smoothingWindow of [ `off | `width of int ]
  | `dataUrl of string
  | `bigDataUrl of string
  | `useScore of bool
  ]

  val url : genome -> option list -> string
end
