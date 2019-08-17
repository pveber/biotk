open Core_kernel

let sum n ~f =
  let rec loop acc i =
    if i = n then acc
    else loop (acc +. f i) (i + 1)
  in
  loop 0. 0
