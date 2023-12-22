open Util

type dimension = { l : int; h : int }

module Block = struct
  type t = { dim_x : dimension; dim_y : dimension; dim_z : dimension }

  let create ((x1, y1, z1), (x2, y2, z2)) =
    {
      dim_x = { l = x1; h = x2 };
      dim_y = { l = y1; h = y2 };
      dim_z = { l = z1; h = z2 };
    }

  let compare b1 b2 =
    match Stdlib.compare b1.dim_z b2.dim_z with
    | 0 -> (
        match Stdlib.compare b1.dim_x b2.dim_x with
        | 0 -> Stdlib.compare b1.dim_y b2.dim_y
        | c -> c)
    | c -> c

  let lower b =
    let dim_z = { l = b.dim_z.l - 1; h = b.dim_z.h - 1 } in
    if dim_z.l > 1 then Some { b with dim_z } else None

  let intersection b1 b2 =
    let inner dim1 dim2 =
      let l = max dim1.l dim2.l and h = min dim1.h dim2.h in
      if l < h then Some { l; h } else None
    in
    let dim_x = inner b1.dim_x b2.dim_x
    and dim_y = inner b1.dim_y b2.dim_y
    and dim_z = inner b1.dim_z b2.dim_z in
    match (dim_x, dim_y, dim_z) with
    | Some dim_x, Some dim_y, Some dim_z -> Some { dim_x; dim_y; dim_z }
    | _ -> None
end

let parse_coord s =
  match sp ',' s |> List.map int_of_string with
  | [ x; y; z ] -> (x, y, z)
  | _ -> assert false

let parse lines =
  lines
  |> List.map
       (Block.create << List.fst << List.pairwise << List.map parse_coord
      << sp '~')

let rec lower_block blocks block =
  match Block.lower block with
  | Some b
    when List.for_all
           (fun b2 -> Option.is_none @@ Block.intersection b b2)
           blocks ->
      lower_block blocks b
  | _ -> block :: blocks

let lower_blocks = List.fold_left lower_block [] << List.sort Block.compare
let settled blocks = List.sort Block.compare (lower_blocks blocks)
let solve_part_1 lines = -1
let solve_part_2 lines = -1

(* tests *)

let data =
  parse
  @@ get_lines
       {|1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9|}

(* change 1->2 if sample data differs by part *)
let%test "day 22 part 1 sample" = test_sample 22 1 solve_part_1 0
let%test "day 22 part 2 sample" = test_sample 22 1 solve_part_2 0
let%test "day 22 part 1" = test_full 22 solve_part_1 0
let%test "day 22 part 2" = test_full 22 solve_part_2 0
