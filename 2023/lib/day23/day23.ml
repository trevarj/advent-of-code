open Util

let start_end grid =
  let s = (0, Option.get @@ Array.find_index (( = ) '.') grid.(0))
  and ylen = Array.length grid - 1
  and xlen = Array.length grid.(0) - 1 in
  let e = (ylen, Option.get @@ Array.find_index (( = ) '.') grid.(xlen)) in
  (s, e)

let slope_dir = function
  | '<' -> Grid.left
  | '>' -> Grid.right
  | '^' -> Grid.up
  | 'v' -> Grid.down
  | _ -> assert false

let is_slope ch = List.mem ch [ '<'; '>'; 'v'; '^' ]
let part1_slope_cond va dir = not (is_slope va && dir <> slope_dir va)
let part2_slope_cond _ _ = true

let dfs grid start exit slope_cond =
  let seen = ref @@ PairSet.empty in
  let rec dfs' v va acc =
    if v = exit then acc
    else (
      seen := PairSet.add v !seen;
      let neighbors =
        Grid.filter_neighbors
          (fun dir pos a ->
            if (a <> '#' && (not @@ PairSet.mem pos !seen)) && slope_cond va dir
            then Some (pos, a)
            else None)
          v grid
      in
      List.fold_left
        (fun that (pos, a) ->
          seen := PairSet.add pos !seen;
          let this = dfs' pos a (acc + 1) in
          seen := PairSet.remove pos !seen;
          if this > that then this else that)
        0 neighbors)
  in
  dfs' start '.' 0

let parse = String.to_2d_array

let solve cond lines =
  let grid = parse lines in
  let s, e = start_end grid in
  dfs grid s e cond

let solve_part_1 = solve part1_slope_cond
let solve_part_2 = solve part2_slope_cond

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 23 part 1 sample" = test_sample 23 1 solve_part_1 94
let%test "day 23 part 2 sample" = test_sample 23 1 solve_part_2 154
let%test "day 23 part 1" = test_full 23 solve_part_1 2222
let%test "day 23 part 2" = test_full 23 solve_part_2 6590
