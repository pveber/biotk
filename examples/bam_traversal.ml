open Core
open Biocaml_unix
open Gzt
open Rresult

let ok_exn = function
  | Ok x -> x
  | Error (`Msg msg) -> failwith msg
  | Error `Parse_error -> failwith "Incorrect format for location"

let ok_exn' = function
  | Ok x -> x
  | Error e -> failwith (Error.to_string_hum e)

let loc_of_al = function
  | { Sam.rname = Some rname ; pos = Some pos ; tlen = Some tlen ; _ } ->
    let lo = pos - 1 in
    let hi = lo + Int.abs tlen in
    Some GLoc.{ chr = rname ; lo ; hi }
  | _ -> None
  
let loc_of_al0 header al =
  let open Result in
  Bam.Alignment0.decode al header >>| loc_of_al

let indexed_traversal ~bam ~bai ~loc =
  let visited = ref 0 in
  let count =
    Bam_iterator.fold0 ~loc ~bam ~bai ~init:0 ~f:(fun n header al ->
        incr visited ;
        match loc_of_al0 header al with
        | Ok (Some loc') ->
          if GLoc.intersects loc loc' then n + 1 else n
        | Ok None -> n
        | Error _ -> assert false
      )
    |> ok_exn
  in
  count, !visited

let full_traversal ~bam ~loc =
  let visited = ref 0 in
  let count =
    ok_exn' @@ Bam.with_file bam ~f:CFStream.(fun _ als ->
        Stream.filter als ~f:Result.(fun al ->
            incr visited ;
            match map al ~f:loc_of_al with
            | Ok (Some loc') ->
              GLoc.intersects loc loc'
            | Ok None -> false
            | Error _ -> assert false
          )
        |> Stream.to_list
        |> List.length
        |> R.ok
      )
  in
  count, !visited

let main ~bam ~bai ~loc () =
  let loc = ok_exn GLoc.(of_string loc) in
  let intersecting, visited = indexed_traversal ~bam ~bai ~loc in
  let intersecting', visited' = full_traversal ~bam ~loc in
  printf "(/\\ %d, visited = %d) VS (/\\ %d, visited = %d)\n" intersecting visited intersecting' visited'
      
let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"BAM traversal demo/test"
    [%map_open
      let bam = flag "--bam" (required file) ~doc:"PATH BAM file"
      and bai = flag "--bai" (required file) ~doc:"PATH BAI file"
      and loc = flag "--loc" (required string) ~doc:"LOC Location CHR:START-END" in
      main ~bam ~bai ~loc ]

let () = Command.run command
