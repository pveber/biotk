(* OASIS_START *)
(* OASIS_STOP *)
open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         (* | After_rules -> *)
         (*     (\* Internal syntax extension *\) *)
         (*     List.iter *)
         (*       (fun base -> *)
         (*          let tag = "pa_" ^ base and file = "src/syntax/pa_" ^ base ^ ".cmo" in *)
         (*          flag ["ocaml"; "compile"; tag] & S[A"-ppopt"; A file]; *)
         (*          flag ["ocaml"; "ocamldep"; tag] & S[A"-ppopt"; A file]; *)
         (*          flag ["ocaml"; "doc"; tag] & S[A"-ppopt"; A file]; *)
         (*          dep ["ocaml"; "ocamldep"; tag] [file]) *)
         (*       ["guizmin.syntax"]; *)
         | _ ->
             ())
