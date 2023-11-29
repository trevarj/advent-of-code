(* arg parsing *)
let usage = "[-a]"
let new_day = ref false
let args = [ ("-a", Arg.Set new_day, "Generate files for next day") ]

(* day template generation *)
let data_list = Sys.readdir "./lib/data/"
let next_day = (Array.length data_list / 2) + 1

let solution_template day =
  let template =
    {|let solve_part_1 input = ""
let solve_part_2 input = ""

(* tests *)
let%test "day %day part 1" = solve_part_1 (Util.open_data %day 1) = "[part 1 answer]"
let%test "day %day part 2" = solve_part_2 (Util.open_data %day 2) = "[part 2 answer]"
|}
  in
  Str.(global_replace (regexp "%day") (string_of_int day) template)

let make_files day =
  let open Out_channel in
  for part = 1 to 2 do
    with_open_text (Printf.sprintf "./lib/data/day%d_part%d" day part) (fun _ ->
        ())
  done;
  with_open_text (Printf.sprintf "./lib/Day%d.ml" day) (fun oc ->
      output_string oc (solution_template day))

let () =
  Arg.parse args (fun _ -> ()) usage;
  if !new_day then make_files next_day
