open Core_kernel

module Base = struct
  type t = char

  let card = 4
  let all = "acgt"
  let to_char x = x

  let of_char = function
    | 'a' | 'A' -> Some 'a'
    | 'c' | 'C' -> Some 'c'
    | 'g' | 'G' -> Some 'g'
    | 't' | 'T' -> Some 't'
    | _ -> None

  let of_char_exn c =
    match of_char c with
    | Some c -> c
    | None -> invalid_arg "Invalid nucleotide"

  let to_int = function
    | 'a' -> 0
    | 'c' -> 1
    | 'g' -> 2
    | 't' -> 3
    | _ -> assert false
end

include Base
include Alphabet.Make(Base)

let a = all.[0]
let c = all.[1]
let g = all.[2]
let t = all.[3]

let complement = function
  | 'a' -> 't'
  | 'c' -> 'g'
  | 'g' -> 'c'
  | 't' -> 'a'
  | _ -> assert false

let random comp =
  if Array.length comp <> 4 then invalid_arg "expected array of size 4" ;
  match Owl.Stats.categorical_rvs comp with
  | 0 -> 'A'
  | 1 -> 'C'
  | 2 -> 'G'
  | 3 -> 'T'
  | _ -> assert false
