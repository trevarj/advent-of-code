open Util

let parse = List.map (sp ' ' >> List.map int_of_string)

let rec diff_list seq =
  if not (List.any (( <> ) 0) seq) then 0
  else
    let last, diff =
      seq |> List.pairwise
      |> List.fold_left_map (fun _ (left, right) -> (right, right - left)) 0
    in
    last + diff_list diff

let solve f lines = lines |> parse |> List.map f |> List.sum
let solve_part_1 = solve diff_list
let solve_part_2 = solve (List.rev >> diff_list)

(* tests *)
let%test "day 9 part 1 sample" = test_sample 9 1 solve_part_1 114
let%test "day 9 part 2 sample" = test_sample 9 2 solve_part_2 2
let%test "day 9 part 1" = test_full 9 solve_part_1 1904165718
let%test "day 9 part 2" = test_full 9 solve_part_2 964
