open Util
module Lookup = Map.Make (String)

let parse_value v =
  let l_r = String.sub v 1 8 |> sp ',' in
  (List.fst l_r, String.trim @@ List.snd l_r)

let parse_map map line =
  let key_val = sp '=' line in
  let key = String.trim @@ List.fst key_val
  and v = parse_value @@ String.trim @@ List.snd key_val in
  map := Lookup.add key v !map

let parse lines =
  let map = ref Lookup.empty in
  let hd = List.hd lines in
  List.iter (fun l -> parse_map map l) ((List.tl >> List.tl) lines);
  (Seq.cycle @@ List.to_seq @@ String.to_chars hd, map)

let lookup k map = Lookup.find k map

let traverse p map start directions =
  let next = ref start in
  1
  + Option.get
      (Seq.find_index
         (fun dir ->
           let pairs = lookup !next !map in
           let dest =
             match dir with
             | 'L' -> fst pairs
             | 'R' -> snd pairs
             | _ -> assert false
           in
           next := dest;
           p dest)
         directions)

let char_last s = s.[String.length s - 1]
let key_p1 = ( = ) "ZZZ"
let key_p2 x = 'Z' = char_last x
let rec gcd a b = match a with 0 -> b | _ -> gcd (b mod a) a
let lcm a b = a / gcd a b * b

let solve_part_1 lines =
  let dirs, map = parse lines in
  string_of_int (traverse key_p1 map "AAA" dirs)

let solve_part_2 lines =
  let dirs, map = parse lines in
  let starts =
    List.filter_map
      (fun (k, _) -> if char_last k = 'A' then Some k else None)
      (Lookup.to_list !map)
  in
  let paths = List.map (fun start -> traverse key_p2 map start dirs) starts in
  string_of_int @@ List.fold_left lcm (List.hd paths) paths

let%test "day 8 part 1 sample" = test_sample 8 1 solve_part_1 "6"
let%test "day 8 part 2 sample" = test_sample 8 2 solve_part_2 "6"
let%test "day 8 part 1" = test_full 8 solve_part_1 "12083"
let%test "day 8 part 2" = test_full 8 solve_part_2 "13385272668829"
