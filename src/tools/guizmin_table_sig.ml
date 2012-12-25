open Guizmin

type 'a format
type 'a file = 'a format Guizmin.file
type 'a file_path = 'a format Guizmin.file_path

module type Row = sig
  type t
  val labels : string list
  val of_array : string array -> t
end

module type Table = sig
  type t
end

module type F = 
  functor (R : Row) -> 
  functor (T : Table) -> 
sig
  val with_rows :
    ?header:bool ->
    ?sep:char ->
    'a file_path -> f:(R.t Stream.t -> 'b) -> 'b

  val load : ?header:bool -> ?sep:char -> 'a file_path -> T.t
end

(* type 'a contents = 'a constraint 'a = < format : 'b ; row : 'c > *)
(* type 'a file_path = 'a contents Guizmin.file_path *)
(* type 'a file = 'a contents Guizmin.file *)

(* module type Row = sig *)
(*   type t *)
(*   val labels : string list *)
(*   val of_array : string array -> t *)
(* end *)

(* module type Format = sig *)
(*   type t *)
(* end *)

(* module type S = sig *)
(*   type format *)
(*   type row *)
(*   type file_path = < row : row ; format : format > contents Guizmin.file_path *)
(*   type file = < row : row ; format : format > contents Guizmin.file *)

(*   val with_file :  *)
(*     ?header:bool -> *)
(*     ?sep:char -> *)
(*     file_path -> f:(row Stream.t -> 'b) -> 'b *)
    
(*   val save :  *)
(*     ?header:bool -> *)
(*     ?sep:char -> *)
(*     string -> row Stream.t -> unit *)
(* end *)

(* module type Wrapper = functor (R : Row) ->  *)




















