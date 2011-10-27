module Guizmin : sig
  type path = string

  type 'a pipeline
  val eval : 'a pipeline -> 'a

  type 'a pipeline

  val v0 : 'a pipeline
  val v1 : ('a -> 'b) -> 'a value -> 'b pipeline
  val v2 : ('a -> 'b -> 'c) -> 'a value -> 'b value -> 'c pipeline

  type 'a file = private string 
  val f0 : 'a file pipeline
  val f1 : ('a -> path -> unit) -> 'a pipeline -> 'b file pipeline
  val f2 : ('a -> 'b -> path -> unit) -> 'a pipeline -> 'b pipeline -> 'c file pipeline

  val merge : 'a pipeline list -> 'a list pipeline
  val expand : ('a -> 'b pipeline list) -> 'a pipeline -> 'b list pipeline
end
=
struct
  type path = string

  type step = {
    target : path ;
    body : unit -> unit ;
  }

  type protocol = 
      Input of path
    | Step of step * protocol list
    | Merge of protocol list
    | Expand of (unit -> protocol list)

  type 'a value = protocol * unit -> 'a


  let rec run = function
      Input path -> assert (Sys.file_exists path)
    | Step (s,deps) -> 
        if not (Sys.file_exists s.target) then (
	  List.iter run deps ;
	  s.body ()
	)
    | Merge l -> List.iter run l

  let eval (protocol, extract) =
    run protocol ;
    extract ()


  let extract_value path () = 
    let ic = open_in path in 
    let v = input_value ic in
    close_in ic ; v

  let save_value f path () = 
    let oc = open_out path in
    output_value oc (f ()) ;
    close_out oc

  let v0 path = (Input path, extract_value path)

  let v1 f p path = 
    Step ({ target = path ; 
	    body = save_value (fun () -> f (eval p)) path },
	  [ fst p ])
    extract_value path

  let v2 f p q path = 
    Step { target = path ; 
	   body = save_value (fun () -> f (eval p) (eval q)) path },
    extract_value path




  type 'a file = string

  let extract_file path () = path

  let f0 path = Input path, extract_file path

  let f1 f p path = 
    Step {
      target = path ; 
      body = fun () -> f (exec p) path
    },
    extract_file path

  let f2 f p q target = 
    Step { 
      target = path ;
      body = fun () -> f (exec p) (exec q) target
    },
    extract_file path

  let merge l = 
    Merge (List.map fst l), 
    fun () -> List.map (fun (_,e) -> e ()) l
end
