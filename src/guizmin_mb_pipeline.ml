let execute_pipeline config_file = ()

let execute_pipeline_term = Cmdliner.(
  let config_file = 
    let doc = "Path to the configuration file"
    and docv = "CONFIG_FILE" in
    Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)
  in
  Term.(pure execute_pipeline $ config_file)
)

let info = Cmdliner.(
  let doc = "Runs a pipeline" in
  let man = [ `S "BUGS"; `P "Email bug reports to <philippe.veber@univ-lyon1.fr>.";] in
  Term.info "guizmin_mb_pipeline" ~version:"0.1" ~doc ~man
)

let () = ignore (Cmdliner.Term.eval (execute_pipeline_term,info))




















