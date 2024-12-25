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
         (fun n -> String.starts_with ~prefix:"day" n)
         (Array.to_list data_list))

let solution_template day =
  let template =
    {|open Util

let solve_part_1 lines = -1
let solve_part_2 lines = -1

(* tests *)
(* change 1->2 if sample data differs by part *)
let%test "day %day part 1 sample" = test_sample %day 1 solve_part_1 0
let%test "day %day part 2 sample" = test_sample %day 1 solve_part_2 0
let%test "day %day part 1" = test_full %day solve_part_1 0
let%test "day %day part 2" = test_full %day solve_part_2 0
|}
  in
  Str.(global_replace (regexp "%day") (string_of_int day) template)

let dune_template day =
  let template =
    {|(library
  (name day%day)
  (libraries Util)
  (inline_tests (deps (source_tree ../data)))
  (preprocess (pps ppx_inline_test)))
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
  Sys.mkdir (Printf.sprintf "./lib/day%d/" day) 0o755;
  with_open_text (Printf.sprintf "./lib/day%d/dune" day) (fun oc ->
      output_string oc (dune_template day));
  with_open_text (Printf.sprintf "./lib/day%d/day%d.ml" day day) (fun oc ->
      output_string oc (solution_template day))

let () =
  Arg.parse args (fun _ -> ()) usage;
  let n = next_day in
  if !new_day && n <= 25 then make_files n
  else print_string "already reached 25th day\n"
