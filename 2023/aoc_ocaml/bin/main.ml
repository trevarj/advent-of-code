(* arg parsing *)
let usage = "[-a]"
let new_day = ref false
let args = [ ("-a", Arg.Set new_day, "Generate files for next day") ]

(* day template generation *)
let data_list = Sys.readdir "./lib/"

let next_day =
  1
  + (List.length
    @@ List.filter
         (fun n -> String.starts_with ~prefix:"Day" n)
         (Array.to_list data_list))

let solution_template day =
  let template =
    {|open Util

let solve_part_1 lines = ""
let solve_part_2 lines = ""

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day %day part 1 sample" = test_sample %day 1 solve_part_1 0
let%test "day %day part 2 sample" = test_sample %day 1 solve_part_2 0
let%test "day %day part 1" = test_full %day solve_part_1 0
let%test "day %day part 2" = test_full %day solve_part_2 0
|}
  in
  Str.(global_replace (regexp "%day") (string_of_int day) template)

let make_files day =
  let open Out_channel in
  with_open_text (Printf.sprintf "./lib/data/day%d.txt" day) (fun _ -> ());
  for part = 1 to 2 do
    with_open_text
      (Printf.sprintf "./lib/data/day%d_part%d_sample.txt" day part) (fun _ ->
        ())
  done;
  with_open_text (Printf.sprintf "./lib/Day%d.ml" day) (fun oc ->
      output_string oc (solution_template day))

let () =
  Arg.parse args (fun _ -> ()) usage;
  if !new_day then make_files next_day
