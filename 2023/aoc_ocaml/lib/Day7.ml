open Util

type card = Ace | King | Queen | Jack | Joker | Number of int

type hand_type =
  | HighCard
  | Pair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind

let card_of_char has_jokers = function
  | 'A' -> Ace
  | 'K' -> King
  | 'Q' -> Queen
  | 'J' when has_jokers -> Joker
  | 'J' -> Jack
  | 'T' -> Number 10
  | c when is_digit c -> Number (int_of_char c - 48)
  | _ -> assert false

let card_rank = function
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Number n -> n
  | Joker -> 1

let card_compare c1 c2 = compare (card_rank c1) (card_rank c2)
let card_eq c1 c2 = 0 = card_compare c1 c2
let convert_jokers card jokers = List.map (fun _ -> card) jokers

let fill_wildcards = function
  | [ [ a ]; b; c; d; [ Joker ] ] -> [ [ a; a ]; b; c; d ]
  | [ (a :: _ as xs); b; c; [ Joker ] ] -> [ a :: xs; b; c ]
  | [ (a :: _ as xs); b; c; [ Joker; Joker ] ] -> [ a :: a :: xs; b; c ]
  | [ (a :: _ as xs); b; [ Joker ] ] -> [ a :: xs; b ]
  | [ (a :: _ as xs); b; [ Joker; Joker ] ] -> [ a :: a :: xs; b ]
  | [ (a :: _ as xs); b; [ Joker; Joker; Joker ] ] -> [ a :: a :: a :: xs; b ]
  | [ (a :: _ as xs); (Joker :: _ as jz) ] -> [ convert_jokers a jz @ xs ]
  | [ Joker :: _ ] -> [ [ Ace; Ace; Ace; Ace ] ]
  | no_change -> no_change

let grouped_hand hand =
  List.sort
    (fun x y ->
      let hy = List.hd y and hx = List.hd x in
      match compare (List.length y) (List.length x) with
      | _ when hy = Joker -> -1
      | _ when hx = Joker -> 1
      | 0 -> card_compare hy hx
      | c -> c)
    (List.group card_eq hand)

let determine_hand has_jokers hand =
  let grouped = grouped_hand hand in
  let grouped = if has_jokers then fill_wildcards grouped else grouped in
  match grouped with
  | [ _; _; _; _; _ ] -> HighCard
  | [ _; _; _; _ ] -> Pair
  | [ a; _; _ ] when List.length a = 2 -> TwoPair
  | [ a; _; _ ] when List.length a = 3 -> ThreeOfAKind
  | [ a; _ ] when List.length a = 3 -> FullHouse
  | [ a; _ ] when List.length a = 4 -> FourOfAKind
  | [ _ ] -> FiveOfAKind
  | _ -> assert false

let hands_assign_type has_jokers =
  List.map (fun (h, b) -> (determine_hand has_jokers h, h, b))

let rec hand_compare = function
  | x :: xs, y :: ys when compare x y = 0 -> hand_compare (xs, ys)
  | x :: _, y :: _ -> card_compare x y
  | [], [] -> 0
  | _ -> assert false

let hand_dual h1 h2 =
  let t1, h1, _ = h1 and t2, h2, _ = h2 in
  match compare t1 t2 with
  | -1 -> -1
  | 1 -> 1
  | 0 -> hand_compare (h1, h2)
  | _ -> assert false

let parse_hand has_jokers line =
  let hand_bid = sp ' ' line in
  let hand =
    List.map (card_of_char has_jokers) (String.to_chars (List.hd hand_bid))
  and bid = (List.tl >> List.hd) hand_bid in
  (hand, int_of_string bid)

let parse has_jokers = List.map (parse_hand has_jokers)

let solve' hands =
  hands |> List.sort hand_dual
  |> List.fold_left (fun (i, sum) (_, _, b) -> (i + 1, (i * b) + sum)) (1, 0)
  |> snd |> string_of_int

let solve_part_1 lines =
  lines |> parse false |> hands_assign_type false |> solve'

let solve_part_2 lines = lines |> parse true |> hands_assign_type true |> solve'

(* tests *)
let%test "day 7 part 1 sample" = test_sample 7 1 solve_part_1 "6440"
let%test "day 7 part 2 sample" = test_sample 7 2 solve_part_2 "5905"
let%test "day 7 part 1" = test_full 7 solve_part_1 "246912307"
let%test "day 7 part 2" = test_full 7 solve_part_2 "246894760"
