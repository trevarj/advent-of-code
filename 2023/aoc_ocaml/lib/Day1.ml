open String

let is_digit = function '1' .. '9' -> true | _ -> false

let rec convert_digit_words = function
  | 'o' :: 'n' :: 'e' :: rest -> '1' :: convert_digit_words ('e' :: rest)
  | 't' :: 'w' :: 'o' :: rest -> '2' :: convert_digit_words ('o' :: rest)
  | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: rest ->
      '3' :: convert_digit_words ('e' :: rest)
  | 'f' :: 'o' :: 'u' :: 'r' :: rest -> '4' :: convert_digit_words rest
  | 'f' :: 'i' :: 'v' :: 'e' :: rest -> '5' :: convert_digit_words ('e' :: rest)
  | 's' :: 'i' :: 'x' :: rest -> '6' :: convert_digit_words rest
  | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: rest ->
      '7' :: convert_digit_words ('n' :: rest)
  | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: rest ->
      '8' :: convert_digit_words ('t' :: rest)
  | 'n' :: 'i' :: 'n' :: 'e' :: rest -> '9' :: convert_digit_words ('e' :: rest)
  | c :: rest -> c :: convert_digit_words rest
  | [] -> []

let to_chars s = s |> to_seq |> List.of_seq
let from_chars s = s |> List.to_seq |> of_seq
let sum list = List.fold_left (fun acc c -> acc + c) 0 list
let combine_chars (a, b) = make 1 a ^ make 1 b

let combine_first_last_digit s =
  let chars = to_chars s in
  let first = List.find is_digit chars
  and last = List.find is_digit (List.rev chars) in
  (first, last) |> combine_chars |> int_of_string

let solve_part_1 lines =
  lines |> List.map combine_first_last_digit |> sum |> string_of_int

let solve_part_2 lines =
  lines
  |> List.map (fun s ->
         s |> to_chars |> convert_digit_words |> from_chars
         |> combine_first_last_digit)
  |> sum |> string_of_int

(* tests *)
let%test "day 1 part 1 sample" = Util.test_sample 1 1 solve_part_1 "142"
let%test "day 1 part 2 sample" = Util.test_sample 1 2 solve_part_2 "281"
let%test "day 1 part 1" = Util.test_full 1 solve_part_1 "54644"
let%test "day 1 part 2" = Util.test_full 1 solve_part_2 "53348"
