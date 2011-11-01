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

  let param_abstract _loc arg body = match arg with
      `lab_param (lid,ty) ->
	<:expr< fun ~ ($lid:lid$ : $lid:ty$) -> $body$ >>
    | `anon_param (lid,ty) ->
	<:expr< fun ($lid:lid$ : $lid:ty$) -> $body$ >>
    | `opt_param (lid,ty,expr) ->
        <:expr< fun ? ($lid:lid$ : $lid:ty$ = $expr$) -> $body$ >>

  let dep_abstract _loc arg body =
    let arg = match arg with 
      | `anon_dep lid -> <:patt< ($lid: lid$ : _) >>
      | `anon_list_dep lid -> <:patt< ($lid: lid$ : _ list) >>
    in <:expr< fun $arg$ -> $body$ >>
    

  let abstract _loc ~params ~deps ~body = 
    List.fold_right (dep_abstract _loc) deps body
    |> List.fold_right (param_abstract _loc) params 


  let param_list _loc params = 
    let aux _loc param accu = 
      let x = match param with
	| `lab_param (lid,"string") | `anon_param (lid,"string") 
	| `opt_param (lid,"string",_) -> <:expr< string $str:lid$ $lid:lid$ >>

	| `lab_param (lid,"int") | `anon_param (lid,"int") 
	| `opt_param (lid,"int",_) -> <:expr< int $str:lid$ $lid:lid$ >>
	| _ -> assert false
      in <:expr< $x$ :: $accu$ >>
    in	  
    List.fold_right (aux _loc) params <:expr< [] >>

  let digest x = Digest.(to_hex (string (Marshal.to_string x [])))

  let string_of_dep = function
    | `anon_dep lid -> lid
    | `anon_list_dep lid -> lid

  let expr_of_dep _loc = function
    | `anon_dep lid -> <:expr< $lid:lid$ >>
    | `anon_list_dep lid -> <:expr< merge $lid:lid$ >>

  let constructor _loc kind deps id body = match kind, deps with 
    | `file, [] -> <:expr< f0 $id$ (fun _path -> $body$) >>
    | `file, [ x ] -> <:expr< f1 $id$ (fun $lid:string_of_dep x$ _path -> $body$) >>
    (* | `file, 2 -> <:expr< f2 >> *)
    | `value, [] -> <:expr< v0 $id$ (fun () -> $body$) >>
    (* | `value, 1 -> <:expr< v1 >> *)
    | `value, [ x ; y ] -> 
      let argx, argy = string_of_dep x, string_of_dep y in
      <:expr< v2 $id$ (fun $lid:argx$ $lid:argy$ -> $body$) $expr_of_dep _loc x$ $expr_of_dep _loc y$ >>
    | _ -> assert false


  let identifier _loc (name, params) deps =
    <:expr< ($str:name$, $param_list _loc params$) >>

  let expand_pipeline kind _loc ~id ~deps ~body =
    abstract _loc
      ~params:(snd id) 
      ~deps
      ~body:(<:expr< Guizmin.($constructor _loc kind deps (identifier _loc id deps) body$) >>)

  let list_of_opt = function
      None -> []
    | Some l -> l

  EXTEND Gram
    GLOBAL: expr;
    expr: LEVEL "top" [
      [ pkind = pipeline_kind ; LIDENT "pipeline"; pname = a_STRING ;
	params = LIST0 param ; 
	deps = OPT [ LIDENT "uses" ; deps = LIST1 dep -> deps ] ; 
	"->" ; body = expr ->
	  expand_pipeline pkind _loc ~id:(pname,params) ~deps:(list_of_opt deps) ~body
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
    pipeline_kind: [
      [ LIDENT "value" -> `value
      | LIDENT "file"  -> `file 
      | LIDENT "dir"   -> `dir ]
    ];
      
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
