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

let arrangements cache grouping springs =
  let rec arrange' cache springs grouping =
    let dropper = function
      | ('?' | '.') :: ss, gs -> arrange' cache ss gs
      | [], [] -> 1
      | _ -> 0
    in
    let rec keeper = function
      | [], 0 :: bs -> arrange' cache [] bs
      | ('?' | '.') :: ss, 0 :: bs -> arrange' cache ss bs
      | ('?' | '#') :: ss, b :: bs -> keeper (ss, (b - 1) :: bs)
      | _ -> 0
    in
    match Hashtbl.find_opt cache (springs, grouping) with
    | Some res -> res
    | None ->
        let res = dropper (springs, grouping) + keeper (springs, grouping) in
        Hashtbl.add cache (springs, grouping) res;
        res
  in
  arrange' cache springs grouping

let solve lines =
  let cache = Hashtbl.create 256 in
  List.fold_right
    (fun (row, grouping) acc -> acc + arrangements cache grouping row)
    lines 0

let parse_line line =
  let line = sp ' ' line in
  let left = String.to_chars @@ List.fst line
  and right = List.filter_map int_of_string_opt (sp ',' @@ List.snd line) in
  (left, right)

let parse lines = lines |> List.map parse_line

let unfold list =
  List.map
    (fun (s, r) ->
      ((List.replicate 5 >> List.intercalate '?') s, List.repeat 5 r))
    list

let solve_part_1 lines = lines |> parse |> solve
let solve_part_2 lines = lines |> parse |> unfold |> solve

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 12 part 1 sample" = test_sample 12 1 solve_part_1 21
let%test "day 12 part 2 sample" = test_sample 12 1 solve_part_2 525152
let%test "day 12 part 1" = test_full 12 solve_part_1 7622
let%test "day 12 part 2" = test_full 12 solve_part_2 4964259839627
