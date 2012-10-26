let sp = Printf.sprintf

let sh fmt = 
  let shell s =
    if Sys.command s != 0 
    then failwith (sp "shell call failed:\n%s\n" s)
  in Printf.ksprintf shell fmt


let bash ?(debug = false) ?(stdout = stdout) ?(stderr = stderr) cmds = 
  let open Shell in
  if debug then List.iter prerr_endline cmds ;
  call [ cmd "bash" [ "-c" ; String.concat " && " cmds ] ]

let load fn = 
  let ic = open_in fn in
  let v  = Marshal.from_channel ic in
  close_in ic ; v

let save fn v = 
  let oc = open_out fn in
  Marshal.to_channel oc v [] ;
  close_out oc










