(** MEME format for motifs. See the
   {{https://meme-suite.org/meme/doc/meme-format.html}specification}
   *)
module Motif_format : sig
  type alphabet = DNA | RNA | PROTEIN

  type strand = Forward | Reverse | Both

  type probability_matrix = {
    alphabet_length : int option ;
    motif_length : int option ;
    nsites : int option ;
    evalue : float option ;
    data : float array array ;
  }

  type t = {
    version : string ;
    alphabet : alphabet option ;
    strands : strand option ;
    background_frequencies : (char * float) array option ;
    name : string ;
    alternate_name : string option ;
    probability_matrix : probability_matrix ;
    url : string option ;
  }

  val to_channel : t -> out_channel -> unit
end
