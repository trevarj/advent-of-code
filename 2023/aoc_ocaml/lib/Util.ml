module IntPair = struct
  type t = int * int

  let create x y : t = (x, y)

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PairSet = Set.Make (IntPair)
module PairMap = Map.Make (IntPair)

module String = struct
  include String

  let of_file path =
    In_channel.with_open_text path (fun ic ->
        String.trim @@ In_channel.input_all ic)

  let to_chars s = s |> String.to_seq |> List.of_seq
  let of_chars s = s |> List.to_seq |> String.of_seq
  let rec repeat n s = match n with 0 -> "" | n -> s ^ repeat (n - 1) s

  let to_2d_array lines =
    List.rev lines
    |> List.fold_left (fun acc line -> to_chars line :: acc) []
    |> List.map Array.of_list |> Array.of_list
end

module Char = struct
  include Char

  let to_int c = int_of_char c - 48
end

module List = struct
  include List

  let fst = function a :: _ -> a | _ -> raise Not_found
  let snd = function _ :: s :: _ -> s | _ -> raise Not_found
  let sum list = List.fold_left (fun acc c -> acc + c) 0 list

  let min list =
    let head = List.hd list and tail = List.tl list in
    List.fold_left min head tail

  let rec any pred = function
    | x :: _ when pred x -> true
    | _ :: xs -> any pred xs
    | [] -> false

  let pairwise list =
    List.of_seq @@ Seq.zip (List.to_seq list) (List.to_seq (List.tl list))

  let group pred list =
    let rec group' acc = function
      | [] -> acc
      | x :: xs ->
          let l, r = List.partition (pred x) xs in
          group' ((x :: l) :: acc) r
    in
    group' [] list

  let last list = List.hd @@ List.rev list

  let split sep list =
    let rec split' acc accs = function
      | [] -> acc :: accs
      | x :: xs when x = sep -> split' [] (acc :: accs) xs
      | x :: xs -> split' (x :: acc) accs xs
    in
    split' [] [] (List.rev list)

  (* keep separators *)
  let splitk sep list =
    let rec split' acc accs = function
      | [] -> acc :: accs
      | x :: xs when x = sep -> split' [] ([ x ] :: acc :: accs) xs
      | x :: xs -> split' (x :: acc) accs xs
    in
    split' [] [] (List.rev list)

  let split_at i list =
    let rev = List.rev in
    let rec s' i acc = function
      | [] -> (rev acc, [])
      | xs when i = 0 -> (rev acc, xs)
      | x :: xs -> s' (i - 1) (x :: acc) xs
    in
    s' i [] list

  let truncate size list =
    let rev = List.rev and len = List.length in
    let rec t' = function
      | _ :: xs as l when len l > size -> t' xs
      | x :: xs -> x :: t' xs
      | [] -> []
    in
    rev @@ t' (rev list)

  let rec zip l r =
    match (l, r) with l :: ls, r :: rs -> (l, r) :: zip ls rs | _ -> []

  let transpose matrix =
    List.of_seq @@ Seq.map List.of_seq @@ Seq.transpose @@ List.to_seq
    @@ List.map List.to_seq matrix

  let to_2d_list lines =
    List.rev lines
    |> List.fold_left (fun acc line -> String.to_chars line :: acc) []

  let rec intercalate sep = function
    | [] -> []
    | [ l ] -> l
    | l :: ls -> l @ (sep :: intercalate sep ls)

  let rec repeat n l = match n with 0 -> [] | n -> l @ repeat (n - 1) l
  let replicate n l = List.init n (fun _ -> l)
  let fold_leftl f l = List.fold_left f (hd l) (tl l)

  let fold_lefti f acc l =
    let _, res =
      List.fold_left (fun (i, acc) a -> (i + 1, f i acc a)) (0, acc) l
    in
    res

  let rec assoc_upsert (key, value) = function
    | [] -> (key, value) :: []
    | (k, _) :: xs when k = key -> (key, value) :: xs
    | x :: xs -> x :: assoc_upsert (key, value) xs

  let rec assoc_remove key = function
    | [] -> []
    | (k, _) :: xs when k = key -> xs
    | x :: xs -> x :: assoc_remove key xs
end

module Array = struct
  include Array

  (* safe get from a matrix *)
  let get_opt x y matrix =
    let width = Array.length matrix.(0) and height = Array.length matrix in
    if y >= 0 && y < height && x >= 0 && x < width then Some matrix.(y).(x)
    else None
end

(* NOTE: all positions are assumed (y, x) based for legibility in the repl
   and because i'm an idiot! *)
module Grid = struct
  type 'a t = 'a array array

  let up = (-1, 0)
  let down = (1, 0)
  let left = (0, -1)
  let right = (0, 1)
  let ( ++ ) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
  let ( -- ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  let move pos dir = pos ++ dir

  let find_map_neighbor pred pos grid =
    let y, x = pos in
    List.find_map
      (fun (y_off, x_off) ->
        let npos = (y + y_off, x + x_off) in
        pred (y_off, x_off) npos @@ Array.get_opt (snd npos) (fst npos) grid)
      [ up; down; left; right ]

  let filter_neighbors pred pos grid =
    let y, x = pos in
    List.filter_map
      (fun (y_off, x_off) ->
        let npos = (y + y_off, x + x_off) in
        match Array.get_opt (snd npos) (fst npos) grid with
        | Some n -> pred (y_off, x_off) npos n
        | None -> None)
      [ up; down; left; right ]

  let neighbors pos grid =
    filter_neighbors (fun dir pos n -> Some (dir, pos, n)) pos grid
end

let sp = String.split_on_char
let get_lines = sp '\n'

let open_data_sample day part =
  let file = Printf.sprintf "../data/day%d_part%d_sample.txt" day part in
  String.of_file file

let open_data day =
  let file = Printf.sprintf "../data/day%d.txt" day in
  String.of_file file

let debug_result got want =
  if got <> want then (
    Printf.printf "got: %d \nexpected: %d \n" got want;
    false)
  else true

let test_sample day part solution_fn want =
  let got = solution_fn (sp '\n' @@ open_data_sample day part) in
  debug_result got want

let test_full day solution_fn want =
  let got = solution_fn (sp '\n' @@ open_data day) in
  debug_result got want

let ( >> ) f g x = g (f x)
let ( << ) f g x = f (g x)
let is_digit = function '0' .. '9' -> true | _ -> false

let print_list f list =
  let rec internal = function
    | n :: ns ->
        f n;
        print_char ';';
        internal ns
    | [] -> ()
  in
  print_char '[';
  internal list;
  print_char ']'

let print_int_list = print_list print_int
let print_char_list = print_list print_char
let fst (f, _) = f
let snd (_, s) = s

let print_tuple (a, b) =
  print_char '(';
  print_int a;
  print_char ',';
  print_int b;
  print_char ')'

let rec chars_intercalate ch = function
  | [] -> []
  | [ c ] -> [ c ]
  | c :: cs -> c :: ch :: chars_intercalate ch cs

let rec shoelace = function
  | (x1, y1) :: (x2, y2) :: xs ->
      ((y1 + y2) * (x2 - x1)) + shoelace ((x2, y2) :: xs)
  | _ -> 0

let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
