open Camlp4

let ( |> ) x f = f x

module Id : Sig.Id =
struct
  let name = "pa_guizmin"
  let version = "1.0"
end

module Make (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax
  open Ast

  let param_abstract _loc arg body =
    let arg = match arg with
	`lab_param (lid,ty) -> 
	  PaLab (_loc,lid, <:patt< ($lid: lid$ : $lid:ty$) >>)
      | `anon_param (lid,ty) -> 
	  <:patt< ($lid: lid$ : $lid:ty$) >>
      | `opt_param (lid,ty,expr) -> 
	  let e = <:expr< ( $expr$ : $lid:ty$) >> in
	  PaOlbi (_loc, "", PaId (_loc, IdLid (_loc, lid)), e)
     in <:expr< fun $arg$ -> $body$ >>

  let dep_abstract _loc arg body =
    let arg = match arg with 
      | `anon_dep lid -> <:patt< ($lid: lid$ : _) >>
      | `anon_list_dep lid -> <:patt< ($lid: lid$ : _ list) >>
    in <:expr< fun $arg$ -> $body$ >>
    

  let abstract _loc ~params ~deps ~body = 
    List.fold_right (dep_abstract _loc) deps body
    |> List.fold_right (param_abstract _loc) params 

  let expand_process _loc ~params ~deps ~body =
    abstract _loc
      ~params ~deps
      ~body:(<:expr< () >>)

  let list_of_opt = function
      None -> []
    | Some l -> l

  EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "top" [
      [ LIDENT "process"; params = LIST0 param ; deps = OPT [ LIDENT "uses" ; deps = LIST1 dep -> deps ] ; "->" ; body = expr ->
	  expand_process _loc ~params ~deps:(list_of_opt deps) ~body
      ]
    ];
    dep: [
      [ lid = LIDENT -> `anon_dep lid 
      | lid = LIDENT ; "*" -> `anon_list_dep lid ]
    ];
    param: [
      [ "~" ; "(" ; lid = LIDENT ; ":" ; ty = param_type ; ")" -> `lab_param (lid,ty)
      | lid = LIDENT ; ":" ; ty = param_type -> `anon_param (lid, ty)
      | "?" ; "(" ; lid = LIDENT ; ":" ; ty = param_type ; "=" ; e = expr ; ")" -> `opt_param (lid,ty,e) ]
    ];
    param_type: [
      [ LIDENT "string" -> "string"
      | LIDENT "int"    -> "int" ]
    ];
      
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
