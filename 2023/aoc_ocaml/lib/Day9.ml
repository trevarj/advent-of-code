open Util

let parse = List.map (sp ' ' >> List.map int_of_string)

let rec diff_list seq =
  if not (any (( <> ) 0) seq) then 0
  else
    let last, diff =
      seq |> pairwise
      |> List.fold_left_map (fun _ (left, right) -> (right, right - left)) 0
    in
    let next_last = diff_list diff in
    last + next_last

let solve_part_1 lines =
  lines |> parse |> List.map diff_list |> sum |> string_of_int

let solve_part_2 lines =
  lines |> parse |> List.map (List.rev >> diff_list) |> sum |> string_of_int

(* tests *)
let%test "day 9 part 1 sample" = test_sample 9 1 solve_part_1 "114"
let%test "day 9 part 2 sample" = test_sample 9 2 solve_part_2 "2"
let%test "day 9 part 1" = test_full 9 solve_part_1 "1904165718"
let%test "day 9 part 2" = test_full 9 solve_part_2 "964"
