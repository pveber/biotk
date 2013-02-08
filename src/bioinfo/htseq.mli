open Guizmin

module Output : sig
  type tabular data = {
    id : string ;
    count : int
  }
  type format
  type file = format Guizmin_table.file
end

val count : ?feature:string -> Sam.file -> Gtf.file -> Output.file
