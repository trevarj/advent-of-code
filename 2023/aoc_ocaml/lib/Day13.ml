open Util

let find_reflection d pattern =
  let diff (x, y) =
    list_zip x y |> List.filter (fun (x, y) -> x <> y) |> List.length
  in
  let rec reflection' = function
    | [], _ -> None
    | l :: ls, [] -> reflection' (ls, [ l ])
    | (l :: ls as left), right ->
        if list_zip left right |> List.map diff |> sum = d then
          Some (List.length right)
        else reflection' (ls, l :: right)
  in
  match reflection' (pattern, []) with Some i -> i | None -> 0

let parse lines = lines |> to_2d_list |> list_split []

let solve d lines =
  let patterns = parse lines in
  string_of_int
  @@ List.fold_left
       (fun acc pattern ->
         acc
         + (find_reflection d pattern * 100)
         + (find_reflection d @@ list_transpose @@ pattern))
       0 patterns

let solve_part_1 = solve 0
let solve_part_2 = solve 1

(* tests *)
let%test "day 13 part 1 sample" = test_sample 13 1 solve_part_1 "405"

(* change 1->2 if sample data differs by part *)
let%test "day 13 part 2 sample" = test_sample 13 1 solve_part_2 "400"
let%test "day 13 part 1" = test_full 13 solve_part_1 "27502"
let%test "day 13 part 2" = test_full 13 solve_part_2 "31947"
