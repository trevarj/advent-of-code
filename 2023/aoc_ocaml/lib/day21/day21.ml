open Util

let find_start grid =
  let y, x =
    grid
    |> Array.find_mapi (fun y ys ->
           Option.map (fun x -> (x, y)) (Array.find_index (fun x -> x = 'S') ys))
    |> Option.get
  in
  grid.(y).(x) <- '.';
  (y, x)

let walk_around grid start max_steps =
  let seen = ref @@ PairSet.empty
  and q = ref @@ Queue.create ()
  and total = ref 0 in
  let neighbor_plots step pos =
    Grid.filter_neighbors
      (fun _ pos a ->
        match a with
        | '.' when not (PairSet.mem pos !seen) -> Some pos
        | _ -> None)
      pos grid
    |> List.map (fun n -> (step, n))
  in
  Queue.add (0, start) !q;
  while not (Queue.is_empty !q) do
    let current_step, u = Queue.pop !q in
    if current_step <= max_steps && (not @@ PairSet.mem u !seen) then (
      seen := PairSet.add u !seen;
      if current_step mod 2 == max_steps mod 2 then total := !total + 1;
      let neighbors = neighbor_plots (current_step + 1) u in
      Queue.add_seq !q (List.to_seq neighbors))
  done;
  !total

let parse lines = String.to_2d_array lines

let solve_part_1 lines =
  let grid = parse lines in
  let start = find_start grid in
  walk_around grid start 64

let mark_grid grid = List.iter (fun (y, x) -> grid.(y).(x) <- 'O')
let solve_part_2 lines = -1

(* tests *)

(* change 1->2 if sample data differs by part *)
let%test "day 21 part 1 sample" = test_sample 21 1 solve_part_1 42

(* let%test "day 21 part 2 sample" = test_sample 21 1 solve_part_2 0 *)
let%test "day 21 part 1" = test_full 21 solve_part_1 3600
(* let%test "day 21 part 2" = test_full 21 solve_part_2 0 *)
