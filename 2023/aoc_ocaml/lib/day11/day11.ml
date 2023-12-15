open Util

let parse lines = lines |> List.to_2d_list

let expand matrix =
  let rec expand' = function
    | [] -> []
    | x :: xs when List.for_all (fun c -> c = '.' || c = '*') x ->
        (Array.to_list @@ Array.make (List.length x) '*') :: expand' xs
    | x :: xs -> x :: expand' xs
  in
  expand' (List.transpose (expand' @@ List.transpose matrix))

let locate_galaxies multiplier matrix =
  let galaxies = ref [] and y_multiplier = ref 0 in
  List.iteri
    (fun y ys ->
      if List.hd ys = '*' then y_multiplier := !y_multiplier + multiplier - 1;
      let x_multiplier = ref 0 in
      List.iteri
        (fun x ch ->
          if ch = '*' then x_multiplier := !x_multiplier + multiplier - 1
          else if ch = '#' then
            galaxies := (x + !x_multiplier, y + !y_multiplier) :: !galaxies)
        ys)
    matrix;
  !galaxies

let rec permutations list =
  let rec p' x xs =
    match xs with
    | [] -> []
    | xx :: [] -> (x, xx) :: []
    | xx :: xxs -> (x, xx) :: p' x xxs
  in
  match list with [] -> [] | x :: xs -> p' x xs @ permutations xs

let distance_sum pairs =
  List.fold_right
    (fun ((x1, y1), (x2, y2)) acc -> acc + (abs (x2 - x1) + abs (y2 - y1)))
    pairs 0

let solve m lines =
  lines |> parse |> expand |> locate_galaxies m |> permutations |> distance_sum

let solve_part_1 = solve 2
let solve_part_2 = solve 100000

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 11 part 1 sample" = test_sample 11 1 solve_part_1 374
let%test "day 11 part 2 sample" = test_sample 11 1 solve_part_2 8200210
let%test "day 11 part 1" = test_full 11 solve_part_1 10276166
let%test "day 11 part 2" = test_full 11 solve_part_2 59877478798
