let sp = String.split_on_char
let get_lines = sp '\n'

let file_to_string path =
  In_channel.with_open_text path (fun ic ->
      String.trim @@ In_channel.input_all ic)

let open_data_sample day part =
  let file = Printf.sprintf "data/day%d_part%d_sample.txt" day part in
  file_to_string file

let open_data day =
  let file = Printf.sprintf "data/day%d.txt" day in
  file_to_string file

let debug_result got want =
  match got = want with
  | true -> true
  | false ->
      print_string @@ "got: " ^ got ^ "\nexpected: " ^ want ^ "\n";
      false

let test_sample day part solution_fn want =
  let got = solution_fn (sp '\n' @@ open_data_sample day part) in
  debug_result got want

let test_full day solution_fn want =
  let got = solution_fn (sp '\n' @@ open_data day) in
  debug_result got want

let ( >> ) f g x = g (f x)
let sum list = List.fold_left (fun acc c -> acc + c) 0 list

let min list =
  let head = List.hd list and tail = List.tl list in
  List.fold_left min head tail

let rec any pred = function
  | x :: _ when pred x -> true
  | _ :: xs -> any pred xs
  | [] -> false

let string_to_chars s = s |> String.to_seq |> List.of_seq
let string_from_chars s = s |> List.to_seq |> String.of_seq
let is_digit = function '0' .. '9' -> true | _ -> false

let print_int_list list =
  let rec internal = function
    | n :: ns ->
        print_int n;
        print_char ';';
        internal ns
    | [] -> ()
  in
  print_char '[';
  internal list;
  print_char ']'

let group pred list =
  let rec group' acc = function
    | [] -> acc
    | x :: xs ->
        let l, r = List.partition (pred x) xs in
        group' ((x :: l) :: acc) r
  in
  group' [] list

let fst (f, _) = f
let snd (_, s) = s
