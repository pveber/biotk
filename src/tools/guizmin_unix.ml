open Guizmin
open GzmUtils

let wget url = 
  f0
    Param.("guizmin.unix.wget[r1]", [string "url" url])
    Shell.(
      fun env path ->
        call [
          cmd "wget" [ "-O" ; path ; url ]
        ]
    )

let gunzip x =
  f1
    ("guizmin.unix.gunzip[r1]", [])
    Shell.(
      fun env (File f) path ->
	sh "gunzip -c %s > %s" f path
    )
    x

let crlf2lf x =
  f1
    ("guizmin.unix.crlf2lf[r2]", [])
    (fun env (File f) path ->
      sh "tr -d '\r' < %s > %s" f path)
    x




















