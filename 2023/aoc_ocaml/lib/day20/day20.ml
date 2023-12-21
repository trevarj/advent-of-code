open Util
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type modtype = FF | Conj | Bcast

type mod' =
  | Broadcast of string list
  | FlipFlop of (bool * string list)
  | Conjunction of (int * StringSet.t * string list)
  | Output

let parse_dests s = s |> sp ',' |> List.map String.trim
let parse_input (input, _, dests) = List.map (fun d -> (d, input)) dests

let parse_line line =
  let splits = line |> sps " -> " in
  match splits with
  | [ m; dests ] -> (
      let dests = parse_dests dests in
      match m with
      | s when ssub s 0 1 = "%" -> (ssub s 1 (sl s - 1), FF, dests)
      | s when ssub s 0 1 = "&" -> (ssub s 1 (sl s - 1), Conj, dests)
      | s -> (s, Bcast, dests))
  | _ -> assert false

let parse = List.map parse_line

(* end parsing *)

let build_inputs mods =
  let map = ref StringMap.empty in
  let inputs = List.((flatten << map parse_input) mods) in
  List.iter
    (fun (name, dest) ->
      map :=
        StringMap.update name
          (fun v ->
            match v with Some v -> Some (dest :: v) | None -> Some [ dest ])
          !map)
    inputs;
  !map

let build_modules inputs mods =
  List.map
    (fun (name, ty, dests) ->
      let m =
        match ty with
        | Bcast -> Broadcast dests
        | FF -> FlipFlop (false, dests)
        | Conj ->
            Conjunction
              ( List.length
                  (Option.value ~default:[] (StringMap.find_opt name inputs)),
                StringSet.empty,
                dests )
      in
      (name, m))
    mods

let simulate mod_map q =
  let rec dispatch src dst msg =
    match StringMap.find_opt dst !mod_map with
    | Some (Broadcast dests) -> send msg dst dests
    | Some (FlipFlop (mode, dests)) when not msg ->
        let out = not mode in
        mod_map := StringMap.add dst (FlipFlop (out, dests)) !mod_map;
        send out dst dests
    | Some (Conjunction (sz, inc, dests)) ->
        let inc = (if msg then StringSet.add else StringSet.remove) src inc in
        mod_map := StringMap.add dst (Conjunction (sz, inc, dests)) !mod_map;
        let out = sz <> StringSet.cardinal inc in
        send out dst dests
    | _ -> ()
  and send msg src dests =
    let enqueue = List.(to_seq << map (fun dst -> (src, dst, msg))) dests in
    Queue.add_seq !q enqueue
  in
  let lows, highs = (ref 0, ref 0) in
  while not (Queue.is_empty !q) do
    let src, dst, msg = Queue.pop !q in
    if msg then highs := !highs + 1 else lows := !lows + 1;
    dispatch src dst msg
  done;
  (!lows, !highs)

let solve_part_1 lines =
  let mods = parse lines in
  let inputs = build_inputs mods in
  let mod_map = ref @@ StringMap.of_list (build_modules inputs mods)
  and q = ref @@ Queue.create ()
  and lows, highs = (ref 0, ref 0) in
  for _ = 1 to 1000 do
    Queue.add ("button", "broadcaster", false) !q;
    let l, h = simulate mod_map q in
    lows := !lows + l;
    highs := !highs + h
  done;
  !lows * !highs

let solve_part_2 lines = -1

(* tests *)

let data =
  parse
  @@ get_lines
       {|broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output|}

let testing =
  let inputs = build_inputs data in
  let mod_map = ref @@ StringMap.of_list (build_modules inputs data)
  and q = ref @@ Queue.create () in
  Queue.add ("button", "broadcaster", false) !q;
  ignore @@ simulate mod_map q;
  (mod_map, q)

(* change 1->2 if sample data differs by part *)
let%test "day 20 part 1 sample" = test_sample 20 1 solve_part_1 11687500

(* let%test "day 20 part 2 sample" = test_sample 20 1 solve_part_2 0 *)
let%test "day 20 part 1" = test_full 20 solve_part_1 980457412
(* let%test "day 20 part 2" = test_full 20 solve_part_2 0 *)
