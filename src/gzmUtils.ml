let sp = Printf.sprintf

let sh fmt = 
  let shell s =
    if Sys.command s != 0 
    then failwith (sp "shell call failed:\n%s\n" s)
  in Printf.ksprintf shell fmt


let bash ?(debug = false) cmds = 
  if debug then List.iter prerr_endline cmds ;
  Shell.(call [
    cmd "bash" [ "-c" ; String.concat " && " cmds ]
  ])














