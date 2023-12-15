open Util

let solve_part_1 lines = 1
let solve_part_2 lines = 1

(* tests *)
(* change 1->2 if sample data differs by part *)

let%test "day 16 part 1 sample" = test_sample 16 1 solve_part_1 0
let%test "day 16 part 2 sample" = test_sample 16 1 solve_part_2 0
let%test "day 16 part 1" = test_full 16 solve_part_1 0
let%test "day 16 part 2" = test_full 16 solve_part_2 0
