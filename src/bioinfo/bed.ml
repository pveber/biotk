open Guizmin
open MBSchema

type 'a ty
type 'a file_path = 'a ty Guizmin_table.file_path
type 'a file = 'a ty Guizmin_table.file

let basic_parse ?header (path : 'a file_path) =
  Guizmin_table.parse ?header 
    (fun (Guizmin_table.Line f) -> 
      Location.make f.(0) (int_of_string f.(1)) (int_of_string f.(2)))
    path

type track
