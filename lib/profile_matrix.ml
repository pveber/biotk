open Core_kernel
open Misc

module type S = sig
  type t = private float array array
  val length : t -> int
  val random : ?alpha:float -> int -> t
  val simulate_sequence : t -> string
  val composition : t -> float array
end

module type Alphabet = sig
  type t
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(A : Alphabet) = struct
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

module DNA = Make(Nucleotide)
