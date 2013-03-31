open Core.Std
open GzmCore

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

let cache_strata (base_directory : base_directory) =
  let cache_dir = Filename.concat (base_directory :> string) "cache" in
  let files = Sys.readdir cache_dir in
  let files_w_stats = Array.map files ~f:(
    fun f -> 
      let path = Filename.concat cache_dir f in
      Unix.((stat path).st_mtime), du path, f
  )
  in
  Array.sort compare files_w_stats ;
  Array.fold files_w_stats ~init:([], 0L) ~f:(
    fun (l, sum) (mtime, size, f) ->
      let sum' = Int64.(sum + size) in
      (f, Unix.localtime mtime, size, sum') :: l, sum'
  )
  |! fst 
  |! List.rev

let cache_stratum (base_directory : base_directory) space =
  let space = Int64.(1024L * 1024L * of_int space) in
  let strata = cache_strata base_directory in
  List.find_map strata ~f:(fun (file, tm, _, sum) -> if sum > space then Some (file, tm, sum) else None)

let cache_stratum_hum (base_directory : base_directory) space =
  match cache_stratum base_directory space with
  | Some (file, tm, space) -> Some (
    file, 
    (Unix.strftime tm "%F"),
    (float Int64.(to_int_exn (space / (1024L * 1024L))) /. 1024.)
  )
  | None -> None
      
let clear_cache (base_directory : base_directory) space =
  let cache_dir = Filename.concat (base_directory :> string) "cache" in
  let space = Int64.(1024L * 1024L * of_int space) in
  let strata = cache_strata base_directory in
  List.filter_map strata ~f:(fun (file, tm, _, sum) -> 
    if sum <= space then Some (Filename.concat cache_dir file) 
    else None
  )
  |! List.iter ~f:(fun f -> GzmUtils.sh "rm -rf %s" f)

type history = ([`used | `requested | `built] * CalendarLib.Printer.Date.t) list

let tag_of_string = function
| "USED" -> `used
| "REQ" -> `requested
| "BUILT" -> `built
| _ -> assert false

let parse_history_line l =
  let tag, stamp = String.lsplit2_exn l ~on:':' in
  tag_of_string tag, CalendarLib.Printer.Date.from_string (String.lstrip stamp)

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
type cache_selection = cached_file list

let less_tagged tag ~during:period ~than:limit x =
  let open CalendarLib in
  let now = Date.today () in
  List.count x.history ~f:(fun (t,stamp) -> t = tag && Date.(Period.nb_days (sub stamp now) < period)) <= limit

let cache_selection
    ?used_less_than ?req_less_than 
    ?bigger_than
    base =
  let default _ = true in
  let used_less_than = 
    Option.value_map used_less_than ~default ~f:(fun (ntimes, period) -> 
      less_tagged `used ~than:ntimes ~during:period
    )
  and req_less_than  = 
    Option.value_map req_less_than ~default ~f:(fun (ntimes, period) -> 
      less_tagged `requested ~than:ntimes ~during:period
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
  Array.filter cached_files ~f:(fun x -> used_less_than x && req_less_than x) 
  |! Array.to_list




















