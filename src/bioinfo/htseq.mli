open Guizmin

module Output : sig
  type tabular data = {
    id : string ;
    count : int
  }
  include module type of Guizmin_table.Make(Row)(Obj)(Table)(Guizmin_table.No_comment_nor_header)
end

val count : ?feature:string -> Sam.file -> Gtf.file -> Output.file
