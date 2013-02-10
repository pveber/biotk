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




















