type 'a or_error = ('a, [`Msg of string]) result

module Error = struct
  let msg s = Error (`Msg s)
  let msgf fmt = Printf.ksprintf msg fmt
end

let ok_or_failwith = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m
