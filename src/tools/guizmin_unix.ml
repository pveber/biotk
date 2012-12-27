open Guizmin
open GzmUtils

let wget url = 
  f0
    "guizmin.unix.wget[r1]"
    [Param.string "url" url]
    Shell.(
      fun env path ->
        call [
          cmd "wget" [ "-O" ; path ; url ]
        ]
    )

let gunzip x =
  f1
    "guizmin.unix.gunzip[r1]"
    []
    x
    Shell.(
      fun env (File f) path ->
	sh "gunzip -c %s > %s" f path
    )

let crlf2lf x =
  f1
    "guizmin.unix.crlf2lf[r2]"
    []
    x
    (fun env (File f) path ->
      sh "tr -d '\r' < %s > %s" f path)



















