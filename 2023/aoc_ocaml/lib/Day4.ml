open List
open Util

let data =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}

let sp = String.split_on_char

let parse_line line =
  line |> sp ':' |> tl |> hd |> sp '|'
  |> map (sp ' ' >> filter_map int_of_string_opt)

let calculate_score = function
  | [ winners; ours ] ->
      fold_left
        (fun acc n ->
          match (acc, mem n winners) with
          | 0, true -> 1
          | _, true -> acc * 2
          | _ -> acc)
        0 ours
  | _ -> assert false

let calculate_matches = function
  | [ winners; ours ] ->
      fold_left (fun acc n -> if mem n winners then acc + 1 else acc) 0 ours
  | _ -> assert false

let slice s e = List.filteri (fun i _ -> i >= s && i <= e)

(* slow recursion *)
(* let calculate_copies cards = *)
(*   let matches = map calculate_matches cards in *)
(*   let rec proliferate i = function *)
(*     | n :: rest -> *)
(*         let i_next = i + 1 in *)
(*         let sub = slice i_next (i + n) matches in *)
(*         n + proliferate i_next sub + proliferate i_next rest *)
(*     | [] -> 0 *)
(*   in *)
(*   proliferate 0 matches + List.length matches *)

let calculate_copies cards =
  let matches = map calculate_matches cards in
  let counts = Array.make (List.length matches) 1 in
  List.iteri
    (fun i n ->
      for j = 0 to n - 1 do
        counts.(i + j + 1) <- counts.(i + j + 1) + counts.(i)
      done)
    matches;
  sum @@ Array.to_list counts

let solve_part_1 lines =
  lines |> map (parse_line >> calculate_score) |> sum |> string_of_int

let solve_part_2 lines =
  lines |> map parse_line |> calculate_copies |> string_of_int

(* tests *)
let%test "day 4 part 1 sample" = test_sample 4 1 solve_part_1 "13"
let%test "day 4 part 2 sample" = test_sample 4 2 solve_part_2 "30"
let%test "day 4 part 1" = test_full 4 solve_part_1 "26426"

(* slow *)
let%test "day 4 part 2" = test_full 4 solve_part_2 "6227972"
