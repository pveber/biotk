let sp = Printf.sprintf

type cmd = string * string list

let string_of_cmd (prog, args) =
  String.concat " " (prog :: args)

let cmd_tokens_of_string s =
  let accept h t =
    if h <> "" then h :: t else t
  in
  let rec aux i current accu =
    if i < 0 then accept current accu
    else 
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' ->
        aux (i - 1) "" (accept current accu)
      | '\'' | '"' | '`' as c ->
        quote c (i - 1) "" (accept current accu)
      | c -> 
        aux (i - 1) (sp "%c%s" c current) accu
  and quote c i current accu =
    if i < 0 then failwith "GzmUtils.cmd_of_string: unfinished quotation"
    else if s.[i] = c then aux (i - 1) "" (current :: accu) (* accepts even empty strings in that case *)
    else quote c (i - 1) (sp "%c%s" s.[i] current) accu
  in
  aux (String.length s - 1) "" []

let cmd_of_string s = match cmd_tokens_of_string s with
  | h :: t -> h, t
  | [] -> failwith "GzmUtils.cmd_of_string: empty command!"

type 'a logger = ('a,unit,string,unit) format4 -> 'a
let null_logger fmt =
  Printf.ksprintf ignore fmt

let sh 
    ?(debug : 'a logger = null_logger) 
    ?(error : 'a logger = null_logger) 
    ?(stdout = stdout) 
    ?(stderr = stderr)
    fmt = 
  let shell s =
    debug "sh call:\n\n%s\n\n" s ;
    try 
      Shell.call
        ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout))
        ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
        [ Shell.cmd "sh" [ "-c" ; s ] ]
    with Shell.Subprocess_error _ -> (
      error "sh call exited with non-zero code:\n\n%s\n\n" s ;
      Core.Std.failwithf "shell call failed:\n%s\n" s ()
    )
  in
  Printf.ksprintf shell fmt

let shout 
    ?(debug : 'a logger = null_logger) 
    ?(error : 'a logger = null_logger) 
    ?(stderr = stderr)
    fmt = 
  let shell s =
    debug "sh call:\n\n%s\n\n" s ;
    let buf = Buffer.create 1024 in
    try 
      Shell.call
        ~stdout:(Shell.to_buffer buf)
        ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
        [ Shell.cmd "sh" [ "-c" ; s ] ] ;
      Buffer.contents buf
    with Shell.Subprocess_error _ -> (
      error "sh call exited with non-zero code:\n\n%s\n\n" s ;
      Core.Std.failwithf "shell call failed:\n%s\n" s ()
    )
  in
  Printf.ksprintf shell fmt


let bash ?(debug = false) ?(stdout = stdout) ?(stderr = stderr) cmds = 
  Shell.call
    ~stdout:(Shell.to_fd (Unix.descr_of_out_channel stdout))
    ~stderr:(Shell.to_fd (Unix.descr_of_out_channel stderr))
    [ Shell.cmd "bash" [ "-c" ; String.concat " && " cmds ] ]

let pipefail cmd1 cmd2 = 
  Printf.sprintf "exec 3>&1; s=$(exec 4>&1 >&3; { %s; echo $? >&4; } | %s) && exit $s" cmd1 cmd2;;

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















