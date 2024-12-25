open Util

type direction = U | D | L | R

let label i = char_of_int (i + 65)
let vector i = function U -> (0, i) | D -> (0, -i) | L -> (-i, 0) | R -> (i, 0)

let build_plan p plan =
  List.rev
  @@ List.fold_left
       (fun acc (dir, dist, color) ->
         let dir, dist = p dir dist color in
         let x1, y1 = vector dist dir in
         match acc with
         | [] -> (x1, y1) :: acc
         | (x, y) :: _ -> (x1 + x, y1 + y) :: acc)
       [ (0, 0) ]
       plan

let parse_direction = function
  | "3" -> U
  | "1" -> D
  | "2" -> L
  | "0" -> R
  | "U" -> U
  | "D" -> D
  | "L" -> L
  | "R" -> R
  | _ -> assert false

let regular_instruction (dir : direction) (dist : int) (_ : string) = (dir, dist)

let hex_instruction (_ : direction) (_ : int) (hex : string) =
  let dist = int_of_string @@ "0x" ^ String.sub hex 0 5
  and dir = String.sub hex 5 1 |> parse_direction in
  (dir, dist)

let parse_color color = String.sub color 2 6

let parse_line line =
  match sp ' ' line with
  | [ dir; dist; color ] ->
      (parse_direction dir, int_of_string dist, parse_color color)
  | _ -> assert false

let parse lines = lines |> List.map parse_line

let solve nodes =
  let perimeter =
    fst
    @@ List.fold_left
         (fun (acc, a) b -> (acc + manhattan_distance a b, b))
         (0, List.hd nodes)
         (List.tl nodes)
  in
  (abs ((shoelace @@ nodes) + perimeter) / 2) + 1

let solve_part_1 lines =
  lines |> parse |> build_plan regular_instruction |> solve

let solve_part_2 lines = lines |> parse |> build_plan hex_instruction |> solve

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 18 part 1 sample" = test_sample 18 1 solve_part_1 62
let%test "day 18 part 2 sample" = test_sample 18 1 solve_part_2 952408144115
let%test "day 18 part 1" = test_full 18 solve_part_1 108909
let%test "day 18 part 2" = test_full 18 solve_part_2 133125706867777
