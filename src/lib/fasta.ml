open Core_kernel.Std
open CFStream

type 'a item = {
  header : string ;
  sequence : 'a
}

type parsing_error = string * int

let chopl s = String.(sub s ~pos:1 ~len:(length s - 1))

type line =
  | Header of string
  | Sequence of string
  | Empty

type 'a op = < of_string : string -> ('a, string) Result.t ;
               to_string : 'a -> string ;
               concat : 'a list -> 'a >

let classify_line = function
  | "" -> Empty
  | s when s.[0] = '>' -> Header (chopl s)
  | s -> Sequence s

let read_line ic =
  Option.map (In_channel.input_line ic) ~f:classify_line

let read_sequence op loc ic =
  let open Result in
  let rec loop loc accu = match read_line ic with
    | None -> return (accu, None)
    | Some (Sequence s) -> (
        match op#of_string s with
        | Ok x -> loop (loc + 1) (x :: accu)
        | Error e -> fail (e, loc)
      )
    | Some l -> return (accu, Some (loc, l))
  in
  loop loc [] >>= function
  | [], _ ->
    fail ("Missing sequence after header", loc)
  | (seqs, cursor) ->
    return (op#concat (List.rev seqs), cursor)


let read_item op (loc, l) ic =
  let open Result in
  match l with
  | Header header -> (
    match read_sequence op (loc + 1) ic with
    | Ok (sequence, cursor) ->
      return { header ; sequence }, cursor
    | Error e -> fail e, None
    )
  | _ -> fail ("expected_header", loc), None

let read_gen op ic =
  let f cursor = Option.map cursor ~f:(fun l -> read_item op l ic) in
  let init = Option.map (read_line ic) ~f:(fun l -> 1, l) in
  Stream.unfold init ~f

let string_ops =
  object
    method of_string x = Result.Ok x
    method to_string x = x
    method concat x = String.concat x
  end

let read = read_gen string_ops

let ints_ops =
  object
    method of_string s =
      try
        String.split ~on:' ' s
        |> List.map ~f:Int.of_string
        |> Result.return
      with Failure _ -> Result.fail "Failed to parse ints"
    method to_string is =
      List.map is ~f:Int.to_string
      |> String.concat ~sep:" "
    method concat = List.concat
  end

let read_ints = read_gen ints_ops
