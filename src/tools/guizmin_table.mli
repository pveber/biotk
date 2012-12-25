open Guizmin

include module type of Guizmin_table_sig

val with_rows :
  (module Row with type t = 'a) ->
  ?header:bool ->
  ?sep:char ->
  'a file_path -> f:('a Stream.t -> 'b) -> 'b
  
val load : 
  (module Row with type t = 'a) ->
  (module Table with type t = 'b) ->
  ?header:bool -> ?sep:char -> 'a file_path -> 'b

module Make : F











(* (\* val map : string -> (int -> string array -> string array) -> 'a file -> 'b file *\) *)

(* (\* val parse : ?header:bool -> ('a,'b) line_parser -> 'a file_path -> 'b BatEnum.t *\) *)

(* module NEWAPI : sig *)
(*   module type Format = sig *)
(*     type row *)
(*     type table *)
(*     type s *)
(*     val stream_of_channel :  *)
(*       ?line_numbers:bool -> *)
(*       ?header:bool -> *)
(*       ?sep:char -> *)
(*       in_channel -> row Stream.t *)

(*   end *)

(*   type ('s,'r,'t) full_format = (module Format with type s = 's and type row = 'r and type table = 't) *)
(*   type 's constrained_format = ('s, 'r, 't) full_format constraint 's = < row : 'r ; table : 't > *)

(*   class type ['a] row = object *)
(*     method row : 'a *)
(*   end *)

(*   class type ['a] table = object *)
(*     method table : 'a *)
(*   end *)

(*   module type S = sig *)
(*     type 'a format = private 'a constrained_format *)
(*     type 'a file_path = 'a format Guizmin.file_path *)
(*     type 'a file = 'a format Guizmin.file *)

(*     val stream_of_channel : 'a #row format -> in_channel -> 'a Stream.t *)
(*     val stream_to_file : ('a #row as 'b) format -> string -> 'a Stream.t -> unit *)
(*     val load : ('a #table as 'b) format -> 'b file_path -> 'a *)
(*     val save : ('a #table as 'b) format -> string -> 'a -> unit *)
(*   end *)

(*   module Impl : sig *)
(*     type 'a format = 'a constrained_format *)
(*     type 'a file_path = 'a format Guizmin.file_path *)
(*     type 'a file = 'a format Guizmin.file *)
        
(*     val stream_of_channel : 'a #row format -> in_channel -> 'a Stream.t *)
(*     val stream_to_file : ('a #row as 'b) format -> string -> 'a Stream.t -> unit *)
(*     val load : ('a #table as 'b) format -> 'b file_path -> 'a *)
(*     val save : ('a #table as 'b) format -> string -> 'a -> unit *)
(*   end *)

(*   include module type of Impl *)
(* end *)



















