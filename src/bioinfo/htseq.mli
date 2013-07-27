open Guizmin

module Output : sig
  type tabular data = {
    id : string ;
    count : int
  }
  include Guizmin_table.S with type row = Row.t and type table = Table.t and type obj = Obj.t and type 'a ty = 'a Row.ty
end

val count : ?feature:string -> Sam.file -> Gtf.file -> Output.file
