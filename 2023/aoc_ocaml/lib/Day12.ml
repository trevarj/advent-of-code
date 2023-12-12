open Util

let is_valid grouping chars =
  let rec group sum acc = function
    | [] -> acc
    | '#' :: [] -> (sum + 1) :: acc
    | '#' :: cs -> group (sum + 1) acc cs
    | '.' :: cs when sum > 0 -> group 0 (sum :: acc) cs
    | _ :: cs -> group sum acc cs
  in
  grouping = group 0 [] chars

let arrangements grouping springs =
  let arrange' memo springs grouping =
    let dropper = function
      | ('?' | '.') :: ss, gs -> memo ss gs
      | [], [] -> 1
      | _ -> 0
    in
    let rec keeper = function
      | [], 0 :: bs -> memo [] bs
      | ('?' | '.') :: ss, 0 :: bs -> memo ss bs
      | ('?' | '#') :: ss, b :: bs -> keeper (ss, (b - 1) :: bs)
      | _ -> 0
    in
    dropper (springs, grouping) + keeper (springs, grouping)
  in
  memo_rec arrange' springs grouping

let solve lines =
  List.fold_right
    (fun (row, grouping) acc -> acc + arrangements grouping row)
    lines 0

let parse_line line =
  let line = sp ' ' line in
  let left = string_to_chars @@ list_first line
  and right = List.filter_map int_of_string_opt (sp ',' @@ list_second line) in
  (left, right)

let parse lines = lines |> List.map parse_line

let unfold list =
  List.map
    (fun (s, r) ->
      ((list_replicate 5 >> list_intercalate '?') s, list_repeat 5 r))
    list

let solve_part_1 lines = lines |> parse |> solve |> string_of_int
let solve_part_2 lines = lines |> parse |> unfold |> solve |> string_of_int

(* tests *)

let data =
  get_lines
    {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}

let%test "day 12 part 1 sample" = test_sample 12 1 solve_part_1 "21"

(* change 1->2 if sample data differs by part *)
let%test "day 12 part 2 sample" = test_sample 12 1 solve_part_2 "525152"
let%test "day 12 part 1" = test_full 12 solve_part_1 "7622"
let%test "day 12 part 2" = test_full 12 solve_part_2 "[todo]"
