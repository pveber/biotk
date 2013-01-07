let sp = Printf.sprintf

type cmd = string * string list

let string_of_cmd (prog, args) =
  String.concat " " (prog :: args)

type 'a logger = ('a,unit,string,unit) format4 -> 'a


let sh fmt = 
  let shell s =
    if Sys.command s != 0 
    then failwith (sp "shell call failed:\n%s\n" s)
  in Printf.ksprintf shell fmt


let bash ?(debug = false) ?(stdout = stdout) ?(stderr = stderr) cmds = 
  let open Shell in
  call [ cmd "bash" [ "-c" ; String.concat " && " cmds ] ]

let load fn = 
  let ic = open_in fn in
  let v  = Marshal.from_channel ic in
  close_in ic ; v

let save fn v = 
  let oc = open_out fn in
  Marshal.to_channel oc v [] ;
  close_out oc

let lines_to_file dest xs =
  BatFile.write_lines dest (BatStream.enum xs)
  (* Core.Std.Out_channel.with_file  *)
  (*   dest  *)
  (*   ~f:(Biocaml.Stream.lines_to_channel xs) *)

let lines_of_file path = 
  BatList.of_enum (BatFile.lines_of path)
  (* Core.Std.In_channel.with_file *)
  (*   path *)
  (*   ~f:Biocaml.Stream.( *)
  (*     fun ic -> to_list (lines_of_channel ic) *)
  (*   ) *)















