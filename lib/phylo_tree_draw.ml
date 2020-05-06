open Core_kernel
open Biotk_croquis.Croquis

type tree =
  | Leaf of { text : string }
  | Node of { children : branch list }
and branch = Branch of { length : float ; tip : tree }

let rec nb_leaves = function
  | Leaf _ -> 1
  | Node { children } ->
    List.fold children ~init:0 ~f:(fun acc (Branch b) ->
        acc + nb_leaves b.tip
      )

let rec tree_height = function
  | Leaf _ -> 0.
  | Node n ->
    List.map n.children ~f:branch_height
    |> List.reduce ~f:Float.max
    |> Option.value ~default:0.
and branch_height (Branch b) = b.length +. tree_height b.tip

let rec tree_depth = function
  | Leaf _ -> 1
  | Node n ->
    List.map n.children ~f:branch_depth
    |> List.reduce ~f:Int.max
    |> Option.value ~default:0
and branch_depth (Branch b) = 1 + tree_depth b.tip

let leaf text = Leaf { text }
let branch length tip = Branch { length ; tip }
let bnode x y = Node { children = [ x ; y ] }
let node children = Node { children }

type tree_vertical_placement = {
  root : float ;
  height : float ;
}

let vertical_tree_layout ~inter_leaf_space ~height ~y children =
  let n_children = List.length children in
  let children_nb_leaves = List.map children ~f:(fun (Branch b) -> nb_leaves b.tip) in
  let total_leaves = List.fold children_nb_leaves ~init:0 ~f:( + ) in
  let usable_height = height -. Float.of_int n_children *. inter_leaf_space in
  let y_start = y +. height /. 2. in
  assert Float.(usable_height > 0.) ;
  List.fold children_nb_leaves ~init:([], y_start) ~f:(fun (acc, y_start) i ->
      let child_height = height *. Float.of_int i /. Float.of_int total_leaves in
      let y = y_start -. child_height /. 2. in
      { root = y ; height = child_height } :: acc,
      y_start -. child_height
    )
  |> fst
  |> List.rev

let rec draw_tree ~inter_leaf_space ~branch_factor ~x ~y ~height = function
  | Leaf { text = t } -> Picture.text ~size:1. ~halign:`left ~valign:`balanced ~x ~y (" " ^ t)
  | Node { children } ->
    let children_layout = vertical_tree_layout ~inter_leaf_space ~height ~y children in
    let children_pic =
      List.map2_exn children children_layout ~f:(fun b tvp ->
          draw_branch ~inter_leaf_space ~branch_factor ~height:tvp.height b ~x ~y:tvp.root
        )
      |> Picture.blend
    in
    let highest_root = (List.hd_exn children_layout).root in
    let lowest_root = (List.last_exn children_layout).root in
    Picture.blend2 children_pic (Picture.path [ (x, highest_root) ; (x, lowest_root) ])

and draw_branch ~inter_leaf_space ~height ~branch_factor ~x ~y (Branch b) =
  let x' = x +. b.length /. branch_factor in
  Picture.blend2
    (draw_tree ~inter_leaf_space ~branch_factor ~height b.tip ~x:x' ~y)
    (Picture.path [ (x, y) ; (x', y) ])

let draw_tree tree =
  let tree_height = tree_height tree in
  let width = Float.of_int (tree_depth tree) in
  let branch_factor = tree_height /. width in
  let nb_leaves = nb_leaves tree in
  let delta = Biotk_croquis.Croquis.Font.(ymax free_sans -. ymin free_sans) in
  let height = 1.02 *. delta *. Float.of_int (nb_leaves + 1) in
  let inter_leaf_space = height -. (Float.of_int nb_leaves +. 1.) *. delta in
  draw_tree ~inter_leaf_space ~branch_factor ~x:0. ~y:0. ~height tree

let draw_branch (Branch b as branch) =
  let tree_height = branch_height branch in
  let width = Float.of_int (branch_depth branch) in
  let branch_factor = tree_height /. width in
  let nb_leaves = nb_leaves b.tip in
  let delta = Biotk_croquis.Croquis.Font.(ymax free_sans -. ymin free_sans) in
  let height = 1.02 *. delta *. Float.of_int (nb_leaves + 1) in
  let inter_leaf_space = height -. (Float.of_int nb_leaves +. 1.) *. delta in
  draw_branch ~inter_leaf_space ~branch_factor ~x:0. ~y:0. ~height branch
