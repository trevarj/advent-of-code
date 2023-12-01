(* arg parsing *)
let usage = "[-a]"
let new_day = ref false
let args = [ ("-a", Arg.Set new_day, "Generate files for next day") ]

(* day template generation *)
let data_list = Sys.readdir "./lib/"
let next_day = Array.length data_list - 2

let solution_template day =
  let template =
    {|let solve_part_1 lines = ""
let solve_part_2 lines = ""

(* tests *)
let%test "day %day part 1 sample" = Util.test_sample %day 1 solve_part_1 "[todo]"
let%test "day %day part 2 sample" = Util.test_sample %day 2 solve_part_2 "[todo]"
let%test "day %day part 1" = Util.test_full %day solve_part_1 "[todo]"
let%test "day %day part 2" = Util.test_full %day solve_part_2 "[todo]"
|}
  in
  Str.(global_replace (regexp "%day") (string_of_int day) template)

let make_files day =
  let open Out_channel in
  with_open_text (Printf.sprintf "./lib/data/day%d" day) (fun _ -> ());
  for part = 1 to 2 do
    with_open_text (Printf.sprintf "./lib/data/day%d_part%d_sample" day part)
      (fun _ -> ())
  done;
  with_open_text (Printf.sprintf "./lib/Day%d.ml" day) (fun oc ->
      output_string oc (solution_template day))

let () =
  Arg.parse args (fun _ -> ()) usage;
  if !new_day then make_files next_day
