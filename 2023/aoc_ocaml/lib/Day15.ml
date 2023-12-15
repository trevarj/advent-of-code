open Util

let hash s =
  s |> String.to_chars
  |> List.fold_left (fun acc ch -> (acc + Char.code ch) * 17 mod 256) 0

type operation =
  | Insert of { label : string; key : int; value : int }
  | Remove of { label : string; key : int }

let parse_op s =
  match sp '-' s with
  | [ k; "" ] -> Remove { label = k; key = hash k }
  | _ -> (
      match sp '=' s with
      | [ k; focal ] ->
          Insert { label = k; key = hash k; value = int_of_string focal }
      | _ -> assert false)

let do_op box = function
  | Insert { label; key; value } ->
      let inner = box.(key) in
      box.(key) <- List.assoc_upsert (label, value) inner;
      box
  | Remove { label; key } ->
      let inner = box.(key) in
      box.(key) <- List.assoc_remove label inner;
      box

let calculate_box_power index box =
  snd
  @@ List.fold_left
       (fun (slot, acc) (_, focal_len) ->
         (slot + 1, acc + (index * slot * focal_len)))
       (1, 0) box

let solve1 strs = strs |> List.map hash |> List.sum

let solve2 strs =
  let box = Array.make 256 [] in
  let ops = List.map parse_op strs in
  let complete = List.fold_left do_op box ops in
  List.fold_left
    (fun (boxi, acc) box -> (boxi + 1, acc + calculate_box_power boxi box))
    (1, 0) (Array.to_list complete)
  |> snd

let parse s = s |> List.fst |> sp ','
let solve_part_1 lines = lines |> parse |> solve1 |> string_of_int
let solve_part_2 lines = lines |> parse |> solve2 |> string_of_int

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 15 part 1 sample" = test_sample 15 1 solve_part_1 "1320"
let%test "day 15 part 2 sample" = test_sample 15 1 solve_part_2 "145"
let%test "day 15 part 1" = test_full 15 solve_part_1 "517551"
let%test "day 15 part 2" = test_full 15 solve_part_2 "286097"
