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

  type ('s,'r,'t) full_format = (module Format with type s = 's and type row = 'r and type table = 't)
  type 's constrained_format = ('s, 'r, 't) full_format constraint 's = < row : 'r ; table : 't >

  class type ['a] row = object
    method row : 'a
  end

  class type ['a] table = object
    method table : 'a
  end

  module type S = sig
    type 'a format = private 'a constrained_format
    type 'a file_path = 'a format Guizmin.file_path
    type 'a file = 'a format Guizmin.file

    val to_stream : ('a #row as 'b) format -> 'b file_path -> 'a Stream.t
    val load : ('a #table as 'b) format -> 'b file_path -> 'a
  end

  module Impl : sig
    type 'a format = 'a constrained_format
    type 'a file_path = 'a format Guizmin.file_path
    type 'a file = 'a format Guizmin.file
        
    val to_stream : ('a #row as 'b) format -> 'b file_path -> 'a Stream.t
    val load : ('a #table as 'b) format -> 'b file_path -> 'a
  end

  include module type of Impl
end



















