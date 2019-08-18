open Core_kernel

module Profile_matrix = Profile_matrix.DNA

module Naive_MEME = struct
  module Simulation = struct
    type t = {
      motif_sequences : string array ;
      background_sequences : string array ;
      pi : float ;
    }

    let make profile ~motif_probability ~n_sequences =
      let n = Profile_matrix.length profile in
      let n_motif_sequences = Float.(to_int (float n_sequences * motif_probability)) in
      let motif_sequences = Array.init n_motif_sequences ~f:(fun _ ->
          Profile_matrix.simulate_sequence profile
        )
      in
      let background_composition = Profile_matrix.composition profile in
      let background_sequences = Array.init (n_sequences - n_motif_sequences) ~f:(fun _ ->
          Dna_sequence.markov0 n background_composition
        )
      in
      { pi = motif_probability ; motif_sequences ; background_sequences }
  end
end
