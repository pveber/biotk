open Guizmin

let wget = 
  file pipeline "guizmin.unix.wget[r1]" url:string -> (
    Shell.(call [
      cmd "wget" [ "-O" ; _path ; url ]
    ])
  )
