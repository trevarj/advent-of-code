open Util

module WNode = struct
  type t = { dist : int; pos : int * int; dir : int * int; streak : int }

  let make dist pos dir streak = { dist; pos; dir; streak }

  let compare n1 n2 =
    match Stdlib.compare n1.dist n2.dist with
    | 0 -> (
        match Stdlib.compare n1.pos n2.pos with
        | 0 -> (
            match Stdlib.compare n1.dir n2.dir with
            | 0 -> Stdlib.compare n1.streak n2.streak
            | c -> c)
        | c -> c)
    | c -> c
end

module Node = struct
  type t = { pos : int * int; dir : int * int; streak : int }

  let make pos dir streak = { pos; dir; streak }

  let compare n1 n2 =
    match Stdlib.compare n1.pos n2.pos with
    | 0 -> (
        match Stdlib.compare n1.dir n2.dir with
        | 0 -> Stdlib.compare n1.streak n2.streak
        | c -> c)
    | c -> c

  let print_node { pos; dir; streak } =
    let x, y = pos and dx, dy = dir in
    Printf.printf "{ pos: (%d, %d); dir: (%d, %d); streak: %d }" x y dx dy
      streak
end

module NodeMap = Map.Make (Node)
module NodeSet = Set.Make (WNode)

let part1_valid _ _ _ new_streak = new_streak <= 3

let part2_valid dir new_dir streak new_streak =
  new_streak <= 10 && (new_dir = dir || streak >= 4 || streak = 0)

let dijkstra p grid =
  let module S = NodeSet in
  let module M = NodeMap in
  let src = (0, 0) in
  let dist = ref M.(empty)
  and q = ref S.(empty |> add (WNode.make 0 src Grid.right 0)) in

  while not (S.is_empty !q) do
    let u = S.min_elt !q in
    q := S.remove u !q;
    if not @@ M.mem (Node.make u.pos u.dir u.streak) !dist then (
      dist := M.add (Node.make u.pos u.dir u.streak) u.dist !dist;
      List.iter
        (fun (dir, pos, w) ->
          let new_streak = if dir <> u.dir then 1 else u.streak + 1 in
          let is_valid = if p u.dir dir u.streak new_streak then true else false
          and isnt_reverse =
            if Grid.( ++ ) u.dir dir = (0, 0) then false else true
          in

          if
            (not @@ M.mem (Node.make pos dir new_streak) !dist)
            && isnt_reverse && is_valid
          then q := S.add (WNode.make (u.dist + w) pos dir new_streak) !q)
        (Grid.neighbors u.pos grid))
  done;
  !dist

let heat_loss target min_streak dist =
  NodeMap.fold
    (fun node v acc ->
      if node.pos = target && node.streak >= min_streak then Int.min acc v
      else acc)
    dist Int.max_int

let parse lines =
  lines |> List.to_2d_list
  |> List.map (Array.of_list << List.map Char.to_int)
  |> Array.of_list

let solve_part_1 lines =
  let grid = lines |> parse in
  dijkstra part1_valid grid
  |> heat_loss (Array.length grid - 1, Array.length grid.(0) - 1) 0

let solve_part_2 lines =
  let grid = lines |> parse in
  dijkstra part2_valid grid
  |> heat_loss (Array.length grid - 1, Array.length grid.(0) - 1) 4

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 17 part 1 sample" = test_sample 17 1 solve_part_1 102
let%test "day 17 part 2 sample" = test_sample 17 1 solve_part_2 94
let%test "day 17 part 1" = test_full 17 solve_part_1 791
let%test "day 17 part 2" = test_full 17 solve_part_2 900
