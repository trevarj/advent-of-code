open Util

type direction = D of int * int | Split of direction * direction
type piece = Empty | MirrorL | MirrorR | SplitH | SplitV

module DirPosPair = struct
  type t = direction * (int * int)

  let compare (x0, y0) (x1, y1) =
    match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end

module DirPosSet = Set.Make (DirPosPair)

let left = D (-1, 0)
let right = D (1, 0)
let up = D (0, -1)
let down = D (0, 1)

let char_to_piece = function
  | '.' -> Empty
  | '\\' -> MirrorR
  | '/' -> MirrorL
  | '|' -> SplitV
  | '-' -> SplitH
  | _ -> assert false

let direction_transform d piece =
  match piece with
  | Empty -> d
  | MirrorR when d = up -> left
  | MirrorR when d = down -> right
  | MirrorR when d = left -> up
  | MirrorR when d = right -> down
  | MirrorL when d = up -> right
  | MirrorL when d = down -> left
  | MirrorL when d = left -> down
  | MirrorL when d = right -> up
  | SplitH when d = up || d = down -> Split (left, right)
  | SplitV when d = up || d = down -> d
  | SplitH when d = left || d = right -> d
  | SplitV when d = left || d = right -> Split (up, down)
  | _ -> assert false

let energize s_dir s_pos grid =
  let seen = ref DirPosSet.empty in
  let rec walk' dir (x, y) =
    match DirPosSet.find_opt (dir, (x, y)) !seen with
    | None -> (
        match Array.get_opt x y grid with
        | None -> ()
        | Some piece -> (
            seen := DirPosSet.add (dir, (x, y)) !seen;
            match direction_transform dir piece with
            | D (x_off, y_off) as dir -> walk' dir (x + x_off, y + y_off)
            | Split (D (x1, y1), D (x2, y2)) ->
                walk' (D (x1, y1)) (x + x1, y + y1);
                walk' (D (x2, y2)) (x + x2, y + y2)
            | _ -> ()))
    | _ -> ()
  in
  walk' s_dir s_pos;
  PairSet.cardinal @@ PairSet.of_list
  @@ List.map (fun (_, pos) -> pos)
  @@ DirPosSet.to_list !seen

let generate_starts grid =
  let height = Array.length grid.(0) and width = Array.length grid in
  let top = List.init width (fun i -> ((i, 0), down))
  and bottom = List.init width (fun i -> ((i, height - 1), up))
  and left = List.init height (fun i -> ((0, i), right))
  and right = List.init height (fun i -> ((width - 1, i), left)) in
  top @ bottom @ left @ right

let parse lines =
  lines |> List.to_2d_list
  |> List.map (Array.of_list << List.map char_to_piece)
  |> Array.of_list

let solve_part_1 lines = lines |> parse |> energize right (0, 0)

let solve_part_2 lines =
  let grid = parse lines in
  let starts = generate_starts grid in
  List.fold_left
    (fun acc (pos, dir) -> max acc @@ energize dir pos grid)
    0 starts

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day 16 part 1 sample" = test_sample 16 1 solve_part_1 46
let%test "day 16 part 2 sample" = test_sample 16 1 solve_part_2 51
let%test "day 16 part 1" = test_full 16 solve_part_1 8034
let%test "day 16 part 2" = test_full 16 solve_part_2 8225
