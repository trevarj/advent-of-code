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
  |> List.map (fun s -> String.to_chars s |> Array.of_list)
  |> Array.of_list

let find_start =
  Array.find_mapi (fun y ys ->
      Option.map (fun x -> (x, y)) (Array.find_index (fun x -> x = 'S') ys))

let find_next_move prev (x, y) tiles =
  List.find_map
    (fun (x_off, y_off) ->
      let current_tile = Option.get (Array.get_opt x y tiles) in
      let next_tile =
        Option.value ~default:'.' (Array.get_opt (x + x_off) (y + y_off) tiles)
      in
      let next_pos = (x + x_off, y + y_off) in
      if
        List.mem next_tile (valid_nexts (x_off, y_off) current_tile)
        && prev <> next_pos
      then Some (x + x_off, y + y_off)
      else None)
    [ north; south; east; west ]

let walk tiles =
  let start = Option.get @@ find_start tiles in
  let rec walk' prev_pos pos loop =
    match find_next_move prev_pos pos tiles with
    | Some next_pos when next_pos = start -> next_pos :: loop
    | Some next_pos -> walk' pos next_pos (next_pos :: loop)
    | None -> assert false
  in
  walk' start start []

let get_loop lines = lines |> parse |> walk

let solve_part_1 lines =
  lines |> get_loop |> List.length |> (Fun.flip Int.div) 2

(*
https://en.wikipedia.org/wiki/Shoelace_formula
https://en.wikipedia.org/wiki/Pick%27s_theorem

TODO: implement using : https://en.wikipedia.org/wiki/Flood_fill
*)
let solve_part_2 lines =
  let loop = get_loop lines in
  (abs (shoelace loop) - List.length loop + 3) / 2

(* tests *)
let%test "day 10 part 1 sample" = test_sample 10 1 solve_part_1 8
let%test "day 10 part 2 sample" = test_sample 10 2 solve_part_2 10
let%test "day 10 part 1" = test_full 10 solve_part_1 6828
let%test "day 10 part 2" = test_full 10 solve_part_2 459
