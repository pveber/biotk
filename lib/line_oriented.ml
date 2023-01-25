open Core


module Parser = struct
  type state =
    | Current_line of
        { n : int
        ; value : Line.t
        }
    | Finished of { n : int }
  (* [n] represents the number of completed lines *)

  let initial_state = Current_line { n = 0; value = Line.empty }

  let line_number = function
    | Current_line { n; value } -> if String.equal (value :> string) "" then n else n + 1
    | Finished { n } -> n

  let step st i =
    match st, i with
    | Finished _, _ -> st, []
    | Current_line { n; value }, None ->
      Finished { n }, if Line.is_empty value then [] else [ value ]
    | Current_line { n; value }, Some input -> (
      match Line.rightmost input with
      | None, line ->
        let value = Line.append value line in
        Current_line { n; value }, []
      | Some left, right -> (
        match Line.parse_string left with
        | [] -> assert false (* [Line.parse_string] never returns an empty list *)
        | h :: t ->
          ( Current_line { n = n + List.length t + 1; value = right }
          , Line.append value h :: t )))
end

let fold_err fn ~init ~f =
  In_channel.with_file fn ~f:(fun ic ->
      let rec loop lno acc =
        match In_channel.input_line ic with
        | None -> Ok acc
        | Some l -> (
            match f acc lno (Line.of_string_unsafe l) with
            | Ok acc' -> loop (succ lno) acc'
            | Error e -> Error e
          )
      in
      loop 0 init
    )

let fold fn ~init ~f =
  fold_err fn ~init ~f:(fun acc lno x -> Ok (f acc lno x))
  |> Rresult.R.get_ok

module type Item = sig
  type t
  val parse : Line.t -> t
  val unparse : t -> string
end

module type S = sig
  type item
  val load : string -> item list
  val fold : string -> init:'a -> f:('a -> item -> 'a) -> 'a

  val save : item list -> string -> unit
end

module Make(Item : Item) = struct
  type item = Item.t

  let load fn =
    In_channel.read_lines fn
    |> List.map ~f:(fun l ->
        Item.parse (Line.of_string_unsafe l)
      )

  let fold fn ~init ~f =
    fold fn ~init ~f:(fun acc _ l ->
        f acc (Item.parse l)
      )

  let save items fn =
    let open Out_channel in
    with_file fn ~f:(fun oc ->
        List.iter items ~f:(fun item ->
            output_string oc (Item.unparse item) ;
            output_char oc '\n'
          )
      )
end
