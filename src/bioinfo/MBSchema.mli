open Biocaml

module Location : sig
  type t = string * Range.t

  val make : string -> int -> int -> t

  val chr : t -> string 
  val st : t -> int 
  val ed : t -> int 
  val size : t -> int

  val overlaps : t -> t -> bool
  val dist : t -> t -> int
  val position : from:t -> t -> int
  val stranded_position : from:t -> strand:[`Sense | `Antisense] -> t -> int

  val relmove : int -> int -> t -> t

  val center : t -> t
  (** [center l] is the tiniest location at the center of [l] that is
      exactly at the same distance from both ends of [l]. It is of
      length 1 if [l] has an odd length and 2 otherwise. *)
    
  val upstream : up:int -> down:int -> [`Sense | `Antisense] -> t -> t

  val to_string : t -> string
  (** String representation of a location, as <chr>:<start>-<end> *)
end

module Transcript : sig
  type t = {
    id : string ;
    gene_id : string ;
    strand : [`Sense | `Antisense] ;
    exons : Location.t list (** Exons are sorted according to their number, not their location *)
  }

  val tss : t -> Location.t
  val position2tss : t -> Location.t -> int
end


module Gene : sig
  type t = {
    id : string ;
    aliases : (string * string) list ;
    transcripts : Transcript.t list
  }

  val symbol : t -> string option
end

module ConfigFile : sig
  open Sexplib.Std

  type t = statement list
  and statement = 
    | Condition of string
    | Sample of sample
    | Model of string
  and sample = {
    sample_id : string ;
    sample_type : sample_type ;
    sample_file : string list ;
    sample_model : string ;
    sample_condition : string ;
  }
  and sample_type =
    | ChIP_seq of string
    | RNA_seq of string

  val load : string -> t
end



















