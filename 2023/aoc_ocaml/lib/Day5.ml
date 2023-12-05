open String
open Util
module IntMap = Map.Make (Int)

type jump_map = { dest : int; src : int; range : int }

let into_jump_map = function
  | [ dest; src; range ] -> { dest; src; range }
  | _ -> assert false

let parse_seeds line =
  line |> split_on_char ':' |> List.rev |> List.hd |> trim |> split_on_char ' '
  |> List.map int_of_string

let parse_maps lines =
  lines
  |> List.map (split_on_char '\n')
  |> List.fold_left
       (fun xs x ->
         match (xs, x) with
         | xs, [ "" ] -> xs
         | xs, [ header ] when ends_with ~suffix:":" header -> [] :: xs
         | x :: xs, [ num ] ->
             ((split_on_char ' ' num |> List.map int_of_string |> into_jump_map)
             :: x)
             :: xs
         | _ -> assert false)
       []

let parse = function
  | seeds :: maps -> (parse_seeds seeds, parse_maps maps)
  | [] -> ([], [])

let rec traverse_map num = function
  | { dest; src; range } :: _ when num >= src && num < src + range ->
      dest + num - src
  | _ :: entries -> traverse_map num entries
  | [] -> num

let traverse_maps key maps =
  List.fold_right (fun m acc -> traverse_map acc m) maps key

let rec pairings = function
  | a :: b :: rest -> (a, a + b) :: pairings rest
  | [] -> []
  | _ -> assert false

let int_list n len =
  let rec build n = function 0 -> [] | i -> n :: build (n + 1) (i - 1) in
  build n len

let solve_part_1 lines =
  let seeds, maps = parse lines in
  List.map (fun seed -> traverse_maps seed maps) seeds |> min |> string_of_int

let solve_part_2 lines = ""

(* tests *)
let%test "day 5 part 1 sample" = test_sample 5 1 solve_part_1 "35"
let%test "day 5 part 2 sample" = test_sample 5 2 solve_part_2 "46"
let%test "day 5 part 1" = test_full 5 solve_part_1 "227653707"
(* let%test "day 5 part 2" = test_full 5 solve_part_2 "[todo]" *)

let data =
  split_on_char '\n'
    {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
