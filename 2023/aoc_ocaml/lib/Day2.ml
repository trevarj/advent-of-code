open Util
open String
open List

type set = Red of int | Green of int | Blue of int
type min_set = { mutable red : int; mutable green : int; mutable blue : int }

let check_set_min min_set set =
  match set with
  | Red n when n > min_set.red -> min_set.red <- n
  | Green n when n > min_set.green -> min_set.green <- n
  | Blue n when n > min_set.blue -> min_set.blue <- n
  | _ -> ()

let is_true = function true -> true | false -> false

let parse_set set =
  match set |> trim |> split_on_char ' ' with
  | [ n; "red" ] -> Red (int_of_string n)
  | [ n; "green" ] -> Green (int_of_string n)
  | [ n; "blue" ] -> Blue (int_of_string n)
  | _ -> assert false

let check_set = function
  | Red n when n <= 12 -> true
  | Green n when n <= 13 -> true
  | Blue n when n <= 14 -> true
  | _ -> false

let parse_sets sets = sets |> split_on_char ',' |> map parse_set

let parse_rounds game =
  game |> split_on_char ':' |> tl |> hd |> trim |> split_on_char ';'

let check_round sets = sets |> parse_sets |> map check_set |> for_all is_true

let check_round_vs_min_set min_set sets =
  let sets = sets |> parse_sets in
  let rec calc_min_set = function
    | s :: rest ->
        check_set_min min_set s;
        calc_min_set rest
    | [] -> ()
  in
  calc_min_set sets

let parse_game_part_1 game =
  let round_strs = parse_rounds game in
  for_all check_round round_strs

let parse_game_part_2 game =
  let round_strs = parse_rounds game
  and min_set = { red = 0; green = 0; blue = 0 } in
  iter (fun r -> check_round_vs_min_set min_set r) round_strs;
  min_set

let solve_part_1 lines =
  lines
  |> mapi (fun i line ->
         match parse_game_part_1 line with true -> i + 1 | false -> 0)
  |> List.sum |> string_of_int

let solve_part_2 lines =
  lines |> map parse_game_part_2
  |> fold_left
       (fun acc min_set -> acc + (min_set.red * min_set.green * min_set.blue))
       0
  |> string_of_int

(* tests *)
let%test "day 2 part 1 sample" = test_sample 2 1 solve_part_1 "8"
let%test "day 2 part 2 sample" = test_sample 2 2 solve_part_2 "2286"
let%test "day 2 part 1" = test_full 2 solve_part_1 "2913"
let%test "day 2 part 2" = test_full 2 solve_part_2 "55593"
