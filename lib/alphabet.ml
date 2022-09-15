open Core

module type S = sig
  type t
  val all : t list
  val card : int
  val to_char : t -> char
  val of_char : char -> t option
  val of_char_exn : char -> t
  val to_int : t -> int
end

module Make(X : sig val letters : string end) = struct
  open X

  let all = String.to_list letters

  let () = if String.is_empty letters then failwith "empty alphabet"

  let () =
    match List.find_a_dup all ~compare:Char.compare with
    | None -> ()
    | Some c -> failwithf "invalid alphabet, %c is repeated" c ()

  let card = String.length letters

  type t = char
  let to_char x = x

  let mask =
    let t = Array.create ~len:Char.(to_int max_value) false in
    String.iter letters ~f:(fun c -> t.(Char.to_int c) <- true) ;
    t

  let of_char c = if mask.(Char.to_int c) then Some c else None
  let of_char_exn c = if mask.(Char.to_int c) then c else failwith "not a letter of alphabet"

  let code =
    let t = Array.create ~len:Char.(to_int max_value) (-1) in
    String.iteri letters ~f:(fun i c -> t.(Char.to_int c) <- i) ;
    t

  let to_int c = code.(Char.to_int c)
end
