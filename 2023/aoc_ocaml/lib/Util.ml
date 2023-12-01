let file_to_string path =
  In_channel.with_open_text path (fun ic ->
      String.trim @@ In_channel.input_all ic)

let open_data_sample day part =
  let file = Printf.sprintf "data/day%d_part%d_sample" day part in
  file_to_string file

let open_data day =
  let file = Printf.sprintf "data/day%d" day in
  file_to_string file

let debug_result got want =
  match got = want with
  | true -> true
  | false ->
      print_string @@ "got: " ^ got ^ "\nexpected: " ^ want ^ "\n";
      false

let test_sample day part solution_fn want =
  let got =
    solution_fn (String.split_on_char '\n' @@ open_data_sample day part)
  in
  debug_result got want

let test_full day solution_fn want =
  let got = solution_fn (String.split_on_char '\n' @@ open_data day) in
  debug_result got want
