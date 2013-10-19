open Core.Std
open Guizmin

let rec du path =
  let open Unix in
  let stats = stat path in
  match stats.st_kind with
  | S_REG -> stats.st_size
  | S_DIR ->
      let contents = Sys.readdir path in
      let sizes = Array.map contents ~f:(fun x -> du (Filename.concat path x)) in
      Array.fold sizes ~init:stats.st_size ~f:Int64.( + )
  | _ -> 0L

type history = ([`used | `requested | `built] * CalendarLib.Printer.Date.t) list

let tag_of_string = function
| "USED" -> `used
| "REQ" -> `requested
| "BUILT" -> `built
| l -> invalid_argf "gzmConsole.tag_of_string: %s" l ()

let parse_history_line l =
  let stamp, tag = String.lsplit2_exn l ~on:':' in
  try
    tag_of_string (String.lstrip tag), CalendarLib.Printer.Date.from_string stamp
  with
  | Invalid_argument _ as e -> raise e
  | e -> raise e (*
    invalid_argf "gzmConsole.parse_history_line: %s" l ()
  *)

let load_history path =
  if Caml.Sys.file_exists path then
    List.map (In_channel.read_lines path) ~f:parse_history_line
  else
    []

type cached_file = {
  name : string ;
  history : history ;
  size : Int64.t
}
type cache_selection = base_directory * cached_file list

let less_tagged tag ~during:(period_start, period_end) ~than:limit x =
  let open CalendarLib in
  let now = Date.today () in
  let n_tags =
    List.count x.history ~f:(fun (t,stamp) ->
      let stamp_date = Date.(Period.nb_days (sub stamp now)) in
      t = tag && period_start <= stamp_date && stamp_date <= period_end
    )
  in
  n_tags <= limit

let cache_selection
    ?used_less_than ?req_less_than
    ?bigger_than
    base =
  let default _ = true in
  let used_less_than =
    Option.value_map used_less_than ~default ~f:(fun (ntimes, period_start, period_end) ->
      less_tagged `used ~than:ntimes ~during:(period_start, period_end)
    )
  and req_less_than  =
    Option.value_map req_less_than ~default ~f:(fun (ntimes, period_start, period_end) ->
      less_tagged `requested ~than:ntimes ~during:(period_start, period_end)
    )
  and bigger_than =
    Option.value_map bigger_than ~default ~f:(fun size_limit ->
      let size_limit = Int64.of_float (1024. *. 1024. *. 1024. *. size_limit) in
      fun x -> x.size >= size_limit
    )
  in
  let files = Sys.readdir (cache_dir base)  in
  let cached_files = Array.map files ~f:(
    fun f ->
      {
        name = f ;
        history = load_history (Filename.concat (history_dir base) f) ;
        size = du (Filename.concat (cache_dir base) f)
      }
  )
  in
  let files =
    Array.filter cached_files ~f:(fun x -> used_less_than x && req_less_than x && bigger_than x)
    |! Array.to_list
  in
  (base, files)

let size_of_selection (_,l) =
  List.fold_left l ~init:0L ~f:(fun accu x -> Int64.(accu + x.size))
  |! (fun sum -> Int64.(sum / (1024L * 1024L) |! to_float) /. 1024.)


let clear_selection (base, l) =
  List.iter l ~f:(fun f ->
    Guizmin_utils.sh "rm -rf %s" (Filename.concat (cache_dir base) f.name)
  )




















