let sp = Printf.sprintf

let bash cmds = 
  Shell.(call [
    cmd "bash" [ "-c" ; String.concat " && " cmds ]
  ])
