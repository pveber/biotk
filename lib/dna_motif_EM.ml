open Core_kernel

let sum n ~f =
  let rec loop acc i =
    if i = n then acc
    else loop (acc +. f i) (i + 1)
  in
  loop 0. 0

module Profile_matrix = struct
  type t = float array array

  let length = Array.length

  let random ?(alpha = 0.5) motif_length =
    let alpha = Array.create ~len:4 alpha in
    Array.init motif_length ~f:(fun _ ->
        Owl.Stats.dirichlet_rvs ~alpha
      )

  let simulate_sequence eps =
    String.init (Array.length eps) ~f:(fun i ->
        Dna_sequence.random_base eps.(i)
      )

  let composition profile =
    let weights = Array.init 4 ~f:(fun j ->
        sum (Array.length profile) ~f:(fun i -> profile.(i).(j))
      ) in
    let total = Owl.Stats.sum weights in
    Array.map weights ~f:(fun w -> w /. total)
end

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
