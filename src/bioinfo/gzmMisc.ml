open Guizmin

let wget = 
  file pipeline "guizmin/wget"
    url:string -> (
      Shell.(call [
	cmd "wget" [ "-O" ; _path ; url ]
      ])
    )
