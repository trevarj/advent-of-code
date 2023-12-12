open Util

let is_valid grouping chars =
  let rec group sum acc = function
    | [] -> acc
    | '#' :: [] -> (sum + 1) :: acc
    | '#' :: cs -> group (sum + 1) acc cs
    | '.' :: cs when sum > 0 -> group 0 (sum :: acc) cs
    | _ :: cs -> group 0 acc cs
  in
  let groups = List.rev @@ group 0 [] chars in
  groups = grouping

let arrangements grouping chars =
  let arrange' self chs acc =
    match chs with
    | [] when is_valid grouping acc -> 1
    | [] -> 0
    | '?' :: xs -> self xs ('.' :: acc) + self xs ('#' :: acc)
    | (('.' | '#') as ch) :: xs -> self xs (ch :: acc)
    | _ -> assert false
  in
  memo_rec arrange' (List.rev chars) []

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
    (fun (s, r) -> ((list_repeat 5 >> list_intercalate '?') s, list_repeat 5 r))
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
let%test "day 12 part 2 sample" = test_sample 12 2 solve_part_2 "525152"
let%test "day 12 part 1" = test_full 12 solve_part_1 "7622"
(* let%test "day 12 part 2" = test_full 12 solve_part_2 "[todo]" *)
