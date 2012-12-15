open Guizmin
open GzmUtils

let wget = 
  file pipeline "guizmin.unix.wget[r1]" url:string -> (
    Shell.(call [
      cmd "wget" [ "-O" ; _path ; url ]
    ])
  )

let gunzip x =
  f1
    ("guizmin.unix.gunzip[r1]", [])
    Shell.(
      fun env (File f) path ->
	sh "gunzip -c %s > %s" f path
    )
    x
