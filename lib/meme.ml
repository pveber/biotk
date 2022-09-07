open Core

module Motif_format = struct
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

  let string_of_alphabet = function
    | DNA -> "ACGT"
    | RNA -> "ACGU"
    | PROTEIN -> "ACDEFGHIKLMNPQRSTVWY"

  let string_of_strand = function
    | Forward -> "+"
    | Reverse -> "-"
    | Both -> "+-"

  let maybe_sprintf o fmt =
    match o with
    | None -> ""
    | Some x -> sprintf fmt x

  let to_channel m oc =
    let fp fmt = Printf.fprintf oc fmt in
    fp "MEME version %s\n" m.version ;
    Option.iter m.alphabet ~f:(fun a -> fp "ALPHABET=%s\n" (string_of_alphabet a)) ;
    Option.iter m.strands ~f:(fun s -> fp "strands: %s\n" (string_of_strand s)) ;
    Option.iter m.background_frequencies ~f:(fun bg ->
        fp "Background letter frequencies\n%s\n" (
          Array.map bg ~f:(fun (c, x) -> sprintf "%c %f" c x)
          |> String.concat_array ~sep:" "
        )
      ) ;
    fp "MOTIF %s%s\n" m.name (maybe_sprintf m.alternate_name " %s") ;
    fp "letter-probability matrix:%s%s%s%s\n%s\n"
      (maybe_sprintf m.probability_matrix.alphabet_length " alength= %d")
      (maybe_sprintf m.probability_matrix.motif_length    " w= %d")
      (maybe_sprintf m.probability_matrix.nsites " nsites= %d")
      (maybe_sprintf m.probability_matrix.evalue " E= %f")
      (Array.map m.probability_matrix.data ~f:(fun row ->
           Array.map row ~f:(sprintf "%f")
           |> String.concat_array ~sep:" "
         )
       |> String.concat_array ~sep:"\n") ;
    Option.iter m.url ~f:(fun u -> fp "URL %s" u)
end
