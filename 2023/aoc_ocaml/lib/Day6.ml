open Util

let parse lines =
  let lines =
    lines
    |> List.map (sp ':' >> List.tl)
    |> List.flatten
    |> List.map (sp ' ' >> List.filter (( <> ) "") >> List.map int_of_string)
  in
  List.combine (List.nth lines 0) (List.nth lines 1)

let parse2 lines =
  let lines =
    lines
    |> List.map (sp ':' >> List.tl)
    |> List.flatten
    |> List.map
         (sp ' '
         >> List.filter (( <> ) "")
         >> List.fold_left ( ^ ) "" >> int_of_string)
  in
  (List.nth lines 0, List.nth lines 1)

(*

distance = (race_time - charge_time) * charge_time

charge_time + (distance / charge_time) = race_time
 => charge_time^2 + distance = race_time * charge_time
    => charge_time^2 = race_time * charge_time - distance

c = charge time
t = race time
d = distance

0 = -c^2 + tc - d
0 = ax^2 + bx + c

a = -1
b = t
c = -d
x = c

x = -b +- sqrt(b^2 - 4ac)
    ---------------------
          2a
c = -t +- sqrt(t^2 - 4d)
    ---------------------
          -2
*)
let quad_solver (t, d) =
  let t = Float.of_int t and d = Float.of_int d in
  let f (t : float) (d : float) pm =
    pm (-1. *. t) (Float.sqrt ((t *. t) -. (4. *. d))) /. -2.0
  in
  ( Int.of_float @@ Float.floor @@ f t d ( +. ),
    Int.of_float @@ Float.ceil @@ f t d ( -. ) )

let solve_part_1' races =
  races
  |> List.map (quad_solver >> fun (l, h) -> h - l - 1)
  |> List.fold_left Int.mul 1 |> string_of_int

let solve_part_1 lines = lines |> parse |> solve_part_1'

let solve_part_2 lines =
  let races = lines |> parse2 in
  solve_part_1' [ races ]

(* tests *)
let%test "day 6 part 1 sample" = test_sample 6 1 solve_part_1 "288"
let%test "day 6 part 2 sample" = test_sample 6 2 solve_part_2 "71503"
let%test "day 6 part 1" = test_full 6 solve_part_1 "449820"

(* just delete the spaces between nums in the input lol *)
let%test "day 6 part 2" = test_full 6 solve_part_2 "42250895"
