open Util

let west = (-1, 0)
let east = (1, 0)
let north = (0, -1)
let south = (0, 1)

let valid_nexts dir = function
  | ('|' | 'L' | 'J' | 'S') when dir = north -> [ '|'; '7'; 'F'; 'S' ]
  | ('|' | '7' | 'F' | 'S') when dir = south -> [ '|'; 'J'; 'L'; 'S' ]
  | ('-' | 'L' | 'F' | 'S') when dir = east -> [ '-'; 'J'; '7'; 'S' ]
  | ('-' | 'J' | '7' | 'S') when dir = west -> [ '-'; 'L'; 'F'; 'S' ]
  | _ -> []

let parse lines =
  lines
  |> List.map (fun s -> string_to_chars s |> Array.of_list)
  |> Array.of_list

let find_start =
  Array.find_mapi (fun y ys ->
      Option.map (fun x -> (x, y)) (Array.find_index (fun x -> x = 'S') ys))

let find_next_move prev (x, y) tiles =
  List.find_map
    (fun (x_off, y_off) ->
      let current_tile = Option.get (matrix_get x y tiles) in
      let next_tile =
        Option.value ~default:'.' (matrix_get (x + x_off) (y + y_off) tiles)
      in
      let next_pos = (x + x_off, y + y_off) in
      if
        List.mem next_tile (valid_nexts (x_off, y_off) current_tile)
        && prev <> next_pos
      then Some (x + x_off, y + y_off)
      else None)
    [ north; south; east; west ]

let walk tiles start =
  let rec walk' prev_pos pos dist =
    match find_next_move prev_pos pos tiles with
    | Some next_pos when next_pos = start -> dist + 1
    | Some next_pos -> walk' pos next_pos (dist + 1)
    | None -> assert false
  in
  walk' start start 0

let solve1 tiles =
  let dist = tiles |> find_start |> Option.get |> walk tiles in
  dist / 2 |> string_of_int

let solve_part_1 lines = lines |> parse |> solve1
let solve_part_2 lines = ""

(* tests *)
let data = get_lines {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let%test "day 10 part 1 sample" = test_sample 10 1 solve_part_1 "8"

(* let%test "day 10 part 2 sample" = test_sample 10 1 solve_part_2 "[todo]" *)
let%test "day 10 part 1" = test_full 10 solve_part_1 "6828"
(* let%test "day 10 part 2" = test_full 10 solve_part_2 "[todo]" *)
