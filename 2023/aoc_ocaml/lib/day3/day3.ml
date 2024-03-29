open List
open Util

let is_symbol = function
  | Some '.' | Some '0' .. '9' | None -> false
  | _ -> true

let is_gear = function Some '*' -> true | _ -> false

let check_adjacent x y arrays pred =
  let rec find_symbol = function
    | (x_off, y_off) :: _
      when Array.get_opt (x + x_off) (y + y_off) arrays |> pred ->
        Some (x + x_off, y + y_off)
    | _ :: rest -> find_symbol rest
    | [] -> None
  in
  find_symbol
    [ (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0) ]

let calculate_parts validator arrays =
  let current_num = ref ""
  and current_symbol = ref None
  and symbol_map = ref PairMap.empty in
  let resolve () =
    (match (int_of_string_opt !current_num, !current_symbol) with
    | Some n, Some coord ->
        let parts =
          match PairMap.find_opt coord !symbol_map with
          | Some l -> n :: l
          | None -> n :: []
        in
        symbol_map := PairMap.add coord parts !symbol_map
    | _, _ -> ());
    current_num := "";
    current_symbol := None
  in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j ch ->
          match is_digit ch with
          | true ->
              current_num := !current_num ^ String.make 1 ch;
              if !current_symbol |> Option.is_none then
                current_symbol := check_adjacent j i arrays validator
          | false -> resolve ())
        row;
      resolve ())
    arrays;
  !symbol_map

let solve validator accumulator lines =
  let map = lines |> String.to_2d_array |> calculate_parts validator in
  PairMap.fold accumulator map 0

let solve_part_1 lines =
  solve is_symbol
    (fun _ v acc -> if length v > 0 then acc + fold_left ( + ) 0 v else acc)
    lines

let solve_part_2 lines =
  solve is_gear
    (fun _ v acc -> if length v = 2 then acc + fold_left ( * ) 1 v else acc)
    lines

(* tests *)
let%test "day 3 part 1 sample" = test_sample 3 1 solve_part_1 4361
let%test "day 3 part 2 sample" = test_sample 3 2 solve_part_2 467835
let%test "day 3 part 1" = test_full 3 solve_part_1 530849
let%test "day 3 part 2" = test_full 3 solve_part_2 84900879
