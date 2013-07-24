open Core.Std
open Guizmin
open Guizmin_bioinfo.MBSchema
module Stream = Biocaml_stream
module Peak = Guizmin_bioinfo.Macs.Wo_control.Peak

type binding_region = {
  chrom : string ;
  summits : int list ;
  center : int ;
  convex_hull : Location.t
}

let centered_location_of_binding_region ~radius br =
  Location.make br.chrom (br.center - radius) (br.center + radius)

let peak_has_enough_tags p = p.Peak.tags > 20
let summit p = Peak.(p.chromStart + p.summit)

let find_clusters_on_chromosome (_,peaks) =
  let peaks = Array.of_list peaks in
  Array.sort ~cmp:(fun p q -> compare (summit p) (summit q)) peaks ;
  let current = ref [ peaks.(0) ] and r = ref [] in
  for i = 1 to Array.length peaks - 1 do 
    if summit peaks.(i) - summit (List.hd_exn !current) > 100 then (
      r := !current :: !r ;
      current := [ peaks.(i) ]
    )
    else 
      current := peaks.(i) :: !current
  done ;
  r := !current :: !r ;
  Stream.of_list (List.rev_map ~f:List.rev !r)

let find_clusters peak_tracks = Stream.(
  of_list peak_tracks
  |! map ~f:(
    fun peaks ->
      filter (of_list peaks) ~f:peak_has_enough_tags
  )
  |! concat
  |! map ~f:(fun p -> p.Peak.chrom, p)
  |! Biocaml_accu.relation
  |! map ~f:find_clusters_on_chromosome
  |! concat
  |! to_list
)

(* a cluster of peaks is a non-empty set of peaks on the same
   chromosome, with no gap larger than 100bp between two consecutive
   summits *)
let binding_region_of_macs_peaks peaks =
  let n = List.length peaks in
  let chrom = (List.hd_exn peaks).Peak.chrom in   (* peaks is non-empty*)
  let summits = List.map peaks ~f:summit in
  let center = List.fold_left summits ~f:( + ) ~init:0 / n in
  {
    summits ; center ; chrom ;
    convex_hull = 
      Location.make 
        chrom 
        (List.fold_left summits ~init:Int.max_value ~f:min)
        (List.fold_left summits ~init:Int.min_value ~f:max)
  }

    
let of_macs_peaks peak_files =
  v1
    "guizmin.bioinfo.labs.binding_region_finder[r1]"
    []
    (Guizmin.merge peak_files)
    (fun env peaks_files ->
      let peak_tracks = List.map peaks_files ~f:(fun file ->
        Guizmin_table.with_rows
          (module Peak.Row)
          file
          ~f:Stream.to_list
      )
      in
      let clusters = find_clusters peak_tracks in
      List.map clusters ~f:binding_region_of_macs_peaks)
