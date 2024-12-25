open Util

type attr = X | M | A | S
type part = { x : int; m : int; a : int; s : int }
type operator = GT | LT
type condition = Cond of { attr : attr; op : operator; limit : int } | NoCond
type destination = Dest of string | A | R
type rule = condition * destination

module RangeMap = Map.Make (String)

let part_of_list = function
  | [ x; m; a; s ] -> { x; m; a; s }
  | _ -> assert false

let part_value part attr =
  match attr with X -> part.x | M -> part.m | A -> part.a | S -> part.s

let part_sum part = part.x + part.m + part.a + part.s

let parse_attr = function
  | "x" -> X
  | "m" -> M
  | "a" -> A
  | "s" -> S
  | _ -> assert false

let parse_cond cond =
  let attr = parse_attr @@ ssub cond 0 1
  and op = match ssub cond 1 1 with "<" -> LT | ">" -> GT | _ -> assert false
  and limit = ssub cond 2 (sl cond - 2) |> int_of_string in
  Cond { attr; op; limit }

let parse_destination = function "A" -> A | "R" -> R | w -> Dest w

let parse_rule rule =
  match sp ':' rule with
  | [ cond; dest ] -> (parse_cond cond, parse_destination dest)
  | [ no_cond ] -> (NoCond, parse_destination no_cond)
  | _ -> assert false

let parse_rules rules = sp ',' rules |> List.map parse_rule

let parse_workflow w =
  match sp '{' w with
  | [ name; rules ] -> (name, parse_rules @@ ssub rules 0 (sl rules - 1))
  | _ -> assert false

let parse_part p =
  ssub p 1 (sl p - 2)
  |> sp ','
  |> List.map (fun r ->
         let r = sp '=' r in
         int_of_string @@ List.snd r)
  |> part_of_list

let parse lines =
  match List.split "" lines with
  | [ workflows; ratings ] ->
      (List.map parse_workflow workflows, List.map parse_part ratings)
  | _ -> assert false

let do_op op v l = match op with GT -> v > l | LT -> v < l

let rec process_rules part (rules : (condition * destination) list) =
  match rules with
  | [] -> assert false
  | (cond, dest) :: rs -> (
      match cond with
      | Cond { attr; op; limit } when do_op op (part_value part attr) limit ->
          dest
      | NoCond -> dest
      | _ -> process_rules part rs)

let get_rules workflow workflows =
  snd @@ List.find (fun (n, _) -> n = workflow) workflows

let process_part workflows part =
  let in_rules = get_rules "in" workflows in
  let rec traverse rules =
    match process_rules part rules with
    | Dest next_wf -> traverse (get_rules next_wf workflows)
    | A -> part_sum part
    | R -> 0
  in
  traverse in_rules

let process_parts (workflows, parts) = List.map (process_part workflows) parts

type range_map = { x : int * int; m : int * int; a : int * int; s : int * int }

let range_product map =
  let xl, xh = map.x and ml, mh = map.m and al, ah = map.a and sl, sh = map.s in
  (xh - xl + 1) * (mh - ml + 1) * (ah - al + 1) * (sh - sl + 1)

let print_range_map map =
  let xl, xh = map.x and ml, mh = map.m and al, ah = map.a and sl, sh = map.s in
  Printf.printf "{ x:(%d, %d); m:(%d, %d); a:(%d, %d); s:(%d, %d); }\n" xl xh ml
    mh al ah sl sh

let split_ranges map attr op limit =
  let split (l, h) limit = function LT -> (l, limit - 1) | GT -> (limit + 1, h)
  and opposite (l, h) limit = function LT -> (limit, h) | GT -> (l, limit) in
  match attr with
  | X ->
      let left = { map with x = split map.x limit op } in
      let right = { map with x = opposite map.x limit op } in
      (left, right)
  | M ->
      let left = { map with m = split map.m limit op } in
      let right = { map with m = opposite map.m limit op } in
      (left, right)
  | A ->
      let left = { map with a = split map.a limit op } in
      let right = { left with a = opposite map.a limit op } in
      (left, right)
  | S ->
      let left = { map with s = split map.s limit op } in
      let right = { left with s = opposite map.s limit op } in
      (left, right)

let combinations workflows =
  let map = { x = (1, 4000); m = (1, 4000); a = (1, 4000); s = (1, 4000) }
  and in_rules = get_rules "in" workflows in
  let rec traverse map rules =
    match rules with
    | [] -> assert false
    | (cond, dest) :: rs -> (
        match (cond, dest) with
        | Cond { attr; op; limit }, Dest dest ->
            let left, right = split_ranges map attr op limit in
            traverse left (get_rules dest workflows) + traverse right rs
        | Cond { attr; op; limit }, A ->
            let left, right = split_ranges map attr op limit in
            range_product left + traverse right rs
        | NoCond, Dest dest -> traverse map (get_rules dest workflows)
        | NoCond, A -> range_product map
        | Cond { attr; op; limit }, R ->
            let _, right = split_ranges map attr op limit in
            traverse right rs
        | NoCond, R -> 0)
  in
  traverse map in_rules

let solve_part_1 lines = lines |> parse |> process_parts |> List.sum
let solve_part_2 lines = lines |> parse |> fst |> combinations

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 19 part 1 sample" = test_sample 19 1 solve_part_1 19114
let%test "day 19 part 2 sample" = test_sample 19 1 solve_part_2 167409079868000
let%test "day 19 part 1" = test_full 19 solve_part_1 398527
let%test "day 19 part 2" = test_full 19 solve_part_2 133973513090020
