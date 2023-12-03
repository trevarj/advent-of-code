open List
open Util

module IntPairs = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PairsMap = Map.Make (IntPairs)

let to_2d_array lines =
  rev lines
  |> fold_left (fun acc line -> string_to_chars line :: acc) []
  |> map Array.of_list |> Array.of_list

let matrix_get x y matrix =
  let width = Array.length matrix.(0) and height = Array.length matrix in
  if x >= 0 && x < height && y >= 0 && y < width then Some matrix.(x).(y)
  else None

let offsets =
  [ (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0) ]

let check_adjacent x y arrays =
  let is_valid = function '.' | '0' .. '9' -> false | _ -> true in
  fold_left
    (fun acc (x_off, y_off) ->
      acc
      || Option.map
           (fun ch -> is_valid ch)
           (matrix_get (x + x_off) (y + y_off) arrays)
         |> Option.value ~default:false)
    false offsets

let check_adjacent_gears x y arrays =
  let is_gear = function Some '*' -> true | _ -> false in
  let rec find_gear = function
    | (x_off, y_off) :: _
      when matrix_get (x + x_off) (y + y_off) arrays |> is_gear ->
        Some (x + x_off, y + y_off)
    | _ :: rest -> find_gear rest
    | [] -> None
  in
  find_gear offsets

let calculate_valid_parts arrays =
  let current_num = ref "" and current_validity = ref false and sum = ref 0 in
  let resolve () =
    (if !current_validity then
       match int_of_string_opt !current_num with
       | None -> ()
       | Some n -> sum := !sum + n);
    current_num := "";
    current_validity := false
  in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j ch ->
          match is_digit ch with
          | true ->
              current_num := !current_num ^ String.make 1 ch;
              current_validity := !current_validity || check_adjacent i j arrays
          | false -> resolve ())
        row;
      resolve ())
    arrays;
  !sum

let calculate_gear_parts arrays =
  let current_num = ref ""
  and current_gear = ref None
  and gear_map = ref PairsMap.empty in
  let resolve () =
    (match (int_of_string_opt !current_num, !current_gear) with
    | Some n, Some coord ->
        let parts =
          match PairsMap.find_opt coord !gear_map with
          | Some l -> n :: l
          | None -> n :: []
        in
        gear_map := PairsMap.add coord parts !gear_map
    | _, _ -> ());
    current_num := "";
    current_gear := None
  in
  Array.iteri
    (fun i row ->
      Array.iteri
        (fun j ch ->
          match is_digit ch with
          | true ->
              current_num := !current_num ^ String.make 1 ch;
              if Option.is_none !current_gear then
                current_gear := check_adjacent_gears i j arrays
          | false -> resolve ())
        row;
      resolve ())
    arrays;
  !gear_map

let solve_part_1 lines =
  lines |> to_2d_array |> calculate_valid_parts |> string_of_int

let solve_part_2 lines =
  let map = lines |> to_2d_array |> calculate_gear_parts in
  PairsMap.fold
    (fun _ v acc ->
      if length v = 2 then acc + fold_left (fun acc i -> i * acc) 1 v else acc)
    map 0
  |> string_of_int

(* tests *)
let%test "day 3 part 1 sample" = test_sample 3 1 solve_part_1 "4361"
let%test "day 3 part 2 sample" = test_sample 3 2 solve_part_2 "467835"
let%test "day 3 part 1" = test_full 3 solve_part_1 "530849"
let%test "day 3 part 2" = test_full 3 solve_part_2 "84900879"
