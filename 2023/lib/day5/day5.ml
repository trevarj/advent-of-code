open String
open Util
module IntMap = Map.Make (Int)

(*
Rewrite and Part 2 solution inspired by https://github.com/byorgey/AoC/blob/master/2023/05/05.hs
*)

type jump_map = { dest : int; src : int; range : int }

let into_jump_map = function
  | [ dest; src; range ] -> { dest; src; range }
  | _ -> assert false

let invert_jump_map { dest; src; range } = { dest = src; src = dest; range }

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

let lookup_entry i { dest; src; range } =
  if i >= src && i < src + range then Some (dest + i - src) else None

let ( <+> ) x y =
  match (x, y) with Some x, _ -> Some x | _, Some y -> Some y | _ -> None

let rec asum lst = match lst with [] -> None | hd :: tl -> hd <+> asum tl

let lookup_map (map : jump_map list) i =
  Option.value (asum (List.map (lookup_entry i) map)) ~default:i

let follow maps x =
  let results = List.map lookup_map maps in
  List.fold_right (fun r acc -> r acc) results x

let rec pairings = function
  | a :: b :: rest -> (a, a + b - 1) :: pairings rest
  | [] -> []
  | _ -> assert false

let in_range i (s, e) = s <= i && i <= e

let solve_part_1 lines =
  let seeds, maps = parse lines in
  List.map (fun seed -> follow maps seed) seeds |> List.min

let solve_part_2 lines =
  let seeds, maps = parse lines in
  let seed_ranges = pairings seeds in
  let maps = List.map (List.map invert_jump_map) (List.rev maps) in
  let rec find i =
    let seed = follow maps i in
    if List.any (in_range seed) seed_ranges then i else find (i + 1)
  in
  find 1

(* tests *)
let%test "day 5 part 1 sample" = test_sample 5 1 solve_part_1 35
let%test "day 5 part 2 sample" = test_sample 5 2 solve_part_2 46
let%test "day 5 part 1" = test_full 5 solve_part_1 227653707
(* let%test "day 5 part 2" = test_full 5 solve_part_2 78775051 *)
