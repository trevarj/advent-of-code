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
let string_of_chars s = s |> List.to_seq |> String.of_seq
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

let print_tuple (a, b) =
  print_char '(';
  print_int a;
  print_char ',';
  print_int b;
  print_char ')'

let list_first = function a :: _ -> a | _ -> raise Not_found
let list_second = function _ :: s :: _ -> s | _ -> raise Not_found

let pairwise list =
  List.of_seq @@ Seq.zip (List.to_seq list) (List.to_seq (List.tl list))

let last list = List.hd @@ List.rev list

(* safe get from a matrix *)
let matrix_get x y matrix =
  let width = Array.length matrix.(0) and height = Array.length matrix in
  if y >= 0 && y < height && x >= 0 && x < width then Some matrix.(y).(x)
  else None

(* helper to convert a string list to a char array array *)
let to_2d_array lines =
  List.rev lines
  |> List.fold_left (fun acc line -> string_to_chars line :: acc) []
  |> List.map Array.of_list |> Array.of_list

let to_2d_list lines =
  List.rev lines
  |> List.fold_left (fun acc line -> string_to_chars line :: acc) []

let list_transpose matrix =
  List.of_seq @@ Seq.map List.of_seq @@ Seq.transpose @@ List.to_seq
  @@ List.map List.to_seq matrix

let rec string_repeat n s =
  match n with 0 -> "" | n -> s ^ string_repeat (n - 1) s

let rec list_repeat n l =
  match n with 0 -> [] | n -> l @ list_repeat (n - 1) l

let rec list_replicate n l =
  match n with 0 -> [] | n -> l :: list_replicate (n - 1) l

let rec chars_intercalate ch = function
  | [] -> []
  | [ c ] -> [ c ]
  | c :: cs -> c :: ch :: chars_intercalate ch cs

let rec list_intercalate sep = function
  | [] -> []
  | [ l ] -> l
  | l :: ls -> l @ (sep :: list_intercalate sep ls)
