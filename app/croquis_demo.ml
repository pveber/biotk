open Base
open Biotk
open Biotk_croquis
open Gg.Color

let tree =
  let open Phylo_tree_draw in
  let leaf = leaf ~style:`italic in
  node [
    branch 0.5 (leaf "Pan troglodytes") ;
    branch 2.5 (
      node ~tag:red [
        branch ~col:red 1. (Phylo_tree_draw.leaf ~col:red "Mus musculus");
        branch ~col:blue 1.5 (leaf ~col:blue "Rattus norvegicus");
      ]
    )
  ]
  |> draw_tree

let pfm =
  let module PFM = Profile_matrix.DNA in
  [|
    [| 0.654 ; 0.045 ; 0.262 ; 0.039 |] ;
    [| 0.019 ; 0.01  ; 0.935 ; 0.036 |] ;
    [| 0.042 ; 0.013 ; 0.673 ; 0.272 |] ;
    [| 0.013 ; 0.074 ; 0.133 ; 0.78  |] ;
    [| 0.01  ; 0.819 ; 0.113 ; 0.058 |] ;
    [| 0.893 ; 0.01  ; 0.068 ; 0.029 |]
  |]
  |> PFM.of_array
  |> Stdlib.Option.get
  |> PFM.draw

let protein_pfm, site_profile1, site_profile2 =
  let module PFM = Profile_matrix.Protein in
  let rng = Gsl.Rng.(make (default ())) in
  let freqs =
    Array.init 10 ~f:(fun i ->
        Profile_matrix.random_profile
          (module Amino_acid) (0.1 /. Float.of_int (i + 1)) rng
      )
  in
  let pfm = Option.value_exn (PFM.of_array freqs) in
  PFM.draw ~palette:PFM.dayhoff_palette pfm, PFM.draw_profile freqs.(0), PFM.draw_profile freqs.(8)

let plot =
  let open Croquis in
  let rng = Gsl.Rng.(make (default ())) in
  let x = Array.init 100 ~f:(fun _ -> Gsl.Rng.uniform rng) in
  let y = Array.init 100 ~f:(fun i -> x.(i) +. Gsl.Randist.gaussian rng ~sigma:0.1) in
  let id = Plot.abline ~thickness:0.01 ~col:Gg.Color.red ~intercept:0. ~slope:1. () in
  let cos_3x = Plot.function_graph ~col:Gg.Color.blue 0. 1. (fun x -> Float.cos (3. *. x)) in
  plot ~xlab:"Signal" ~ylab:"Response" [ Plot.points x y ; cos_3x ; id ]

let picture =
  Croquis.vstack ~align:`centered [
    tree ;
    pfm ;
    pfm ;
    protein_pfm ;
    Croquis.(palette (Colormap.hsl ~lightness:0.5 ~saturation:1. 20)) ;
    Croquis.(palette Profile_matrix.Protein.dayhoff_palette) ;
    site_profile1 ;
    site_profile2 ;
    plot ;
  ]

let () =
  Croquis.render picture `pdf (`File "croquis_demo.pdf")
