open Guizmin

type 'a line = private Line of string array
type ('a, 'b) line_parser = 'a line -> 'b
type 'a file_path = 'a line Guizmin.file_path
type 'a file = 'a line Guizmin.file

val map : string -> (int -> string array -> string array) -> 'a file -> 'b file

val parse : ?header:bool -> ('a,'b) line_parser -> 'a file_path -> 'b BatEnum.t

module NEWAPI : sig
  module type Format = sig
    type row
    type table
    (* Signature *)
    type s
  end

  type ('s,'r,'t) full_ty = (module Format with type s = 's and type row = 'r and type table = 't)
  type 's ty = ('s, 'r, 't) full_ty constraint 's = < row : 'r ; table : 't >

  class type ['a] row = object
    method row : 'a
  end

  class type ['a] table = object
    method table : 'a
  end
    
  type 'a file_path = 'a ty Guizmin.file_path
  type 'a file = 'a ty Guizmin.file
        
  val to_stream : ('a #row as 'b) ty -> 'b file_path -> 'a Stream.t
  val load : ('a #table as 'b) ty -> 'b file_path -> 'a

end

















