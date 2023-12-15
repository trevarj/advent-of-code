open List
open Util

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
let take n = List.filteri (fun i _ -> i < n)

(* my slow recursive solution *)
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

(* glguy's smart haskell solution *)
let calculate_copies cards =
  fold_right (fun m acc -> (1 + List.sum (take m acc)) :: acc) cards []

let solve_part_1 lines =
  lines |> map (parse_line >> calculate_score) |> List.sum |> string_of_int

let solve_part_2 lines =
  lines
  |> map (parse_line >> calculate_matches)
  |> calculate_copies |> List.sum |> string_of_int

(* tests *)
let%test "day 4 part 1 sample" = test_sample 4 1 solve_part_1 "13"
let%test "day 4 part 2 sample" = test_sample 4 2 solve_part_2 "30"
let%test "day 4 part 1" = test_full 4 solve_part_1 "26426"
let%test "day 4 part 2" = test_full 4 solve_part_2 "6227972"
