open Util

type thing = R | H | G

let char_to_thing = function
  | 'O' -> R
  | '#' -> H
  | '.' -> G
  | _ -> assert false

let thing_to_char = function R -> 'O' | H -> '#' | G -> '.'
let print_thing_list ts = ts |> List.map thing_to_char |> print_char_list

let thing_compare d a b =
  match (a, b) with R, G -> -1 * d | G, R -> 1 * d | _ -> 0

let weigh_row row =
  let len = List.length row in
  List.mapi (fun i x -> if x = R then len - i else 0) row |> List.sum

let shift_row dir row =
  row |> List.splitk H
  |> List.map (List.sort @@ thing_compare dir)
  |> List.flatten

let shift dir = List.map (shift_row dir)
let cycle_step dir cycle = cycle |> List.transpose |> shift dir

let cycle m =
  m |> shift 1 |> cycle_step 1 |> cycle_step (-1) |> cycle_step (-1)
  |> List.transpose

let do_cycles n m =
  let cache = Hashtbl.create 256 in
  let rec until acc = function
    | i when i = n -> assert false
    | i -> (
        match Hashtbl.find_opt cache acc with
        | Some start -> (start, i)
        | None ->
            Hashtbl.add cache acc i;
            until (cycle acc) (i + 1))
  in
  let start, next = until m 0 in
  let final = start + ((n - start) mod (next - start)) in
  Option.get
  @@ Seq.find_map
       (fun (k, v) -> if v = final then Some k else None)
       (Hashtbl.to_seq cache)

let parse lines =
  lines |> List.to_2d_list |> List.transpose
  |> List.map (List.map char_to_thing)

let solve_part_1 lines =
  parse lines
  |> List.map (shift_row 1 >> weigh_row)
  |> List.sum |> string_of_int

let solve_part_2 lines =
  parse lines |> do_cycles 1000000000 |> List.map weigh_row |> List.sum
  |> string_of_int

(* tests *)
let%test "day 14 part 1 sample" = test_sample 14 1 solve_part_1 "136"

(* change 1->2 if sample data differs by part *)
let%test "day 14 part 2 sample" = test_sample 14 1 solve_part_2 "64"
let%test "day 14 part 1" = test_full 14 solve_part_1 "102497"
let%test "day 14 part 2" = test_full 14 solve_part_2 "105008"
