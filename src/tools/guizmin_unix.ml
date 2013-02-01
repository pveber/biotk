open Guizmin
open GzmUtils

let wget url = 
  f0
    "guizmin.unix.wget[r1]"
    [Param.string "url" url]
    (
      fun env path ->
        env.sh "wget -O %s %s" path url
    )

let gunzip x =
  f1
    "guizmin.unix.gunzip[r1]"
    []
    x
    (
      fun env (File f) path ->
	env.sh "gunzip -c %s > %s" f path
    )

let unzip x =
  d1
    "guizmin.unix.unzip[r1]"
    []
    x
    Shell.(
      fun env (File f) path ->
	env.sh "unzip -d %s %s" path f
    )

let crlf2lf x =
  f1
    "guizmin.unix.crlf2lf[r2]"
    []
    x
    (fun env (File f) path ->
      sh "tr -d '\r' < %s > %s" f path)



















