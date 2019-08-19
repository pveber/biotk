type t = char

let card = 4

let a = 'a'
let c = 'c'
let g = 'g'
let t = 't'

let all = [a;c;g;t]

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

let complement = function
  | 'a' -> 't'
  | 'c' -> 'g'
  | 'g' -> 'c'
  | 't' -> 'a'
  | _ -> assert false
