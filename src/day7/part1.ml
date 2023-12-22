open Utils

let day = D7

type hand_type =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPairs
  | OnePair
  | HighCard
[@@deriving show { with_path = false }]

type hand = { hand_type : hand_type; hand : int list; bid : int }
[@@deriving show { with_path = false }]

let hand_type_value = function
  | FiveOfAKind -> 7
  | FourOfAKind -> 6
  | FullHouse -> 5
  | ThreeOfAKind -> 4
  | TwoPairs -> 3
  | OnePair -> 2
  | HighCard -> 1

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let compare_hands (h1 : hand) (h2 : hand) : int =
  let cmp =
    compare (hand_type_value h1.hand_type) (hand_type_value h2.hand_type)
  in
  if cmp <> 0 then cmp
  else
    let cmp = compare (List.hd h1.hand) (List.hd h2.hand) in
    if cmp <> 0 then cmp
    else
      let cmp = compare (List.nth h1.hand 1) (List.nth h2.hand 1) in
      if cmp <> 0 then cmp
      else
        let cmp = compare (List.nth h1.hand 2) (List.nth h2.hand 2) in
        if cmp <> 0 then cmp
        else
          let cmp = compare (List.nth h1.hand 3) (List.nth h2.hand 3) in
          if cmp <> 0 then cmp
          else compare (List.nth h1.hand 4) (List.nth h2.hand 4)

let solve (input : hand list) : string =
  let sorted_hands = List.sort compare_hands input in
  let winnings = List.mapi (fun i h -> h.bid * (i + 1)) sorted_hands in
  List.fold_left ( + ) 0 winnings |> string_of_int
(* List.iter
   (fun x ->
     List.iter
       (fun y ->
         let cmp = compare_hands x y in
         Printf.printf "%s vs %s = %d\n" (show_hand x) (show_hand y) cmp)
       input)
   input; *)

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let count_char (hand : int list) (c : int) : int =
  List.fold_left (fun acc x -> if x = c then acc + 1 else acc) 0 hand

let count_chars (hand : int list) : (int * int) list =
  let rec aux hand acc =
    match hand with
    | [] -> acc
    | h :: _ ->
        let count = count_char hand h in
        aux (List.filter (fun x -> x <> h) hand) ((h, count) :: acc)
  in
  aux hand []

let max_count (counts : (int * int) list) : int =
  List.fold_left (fun acc (_, count) -> max acc count) 0 counts

let is_two_pairs (counts : (int * int) list) : bool =
  let rec aux counts acc =
    match counts with
    | [] -> acc = 2
    | (_, count) :: t -> aux t (if count = 2 then acc + 1 else acc)
  in
  aux counts 0

let has_one_pair (counts : (int * int) list) : bool =
  let rec aux counts =
    match counts with
    | [] -> false
    | (_, count) :: t -> if count = 2 then true else aux t
  in
  aux counts

let map = function
  | 'T' -> 10
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | c -> Char.escaped c |> int_of_string

let string_to_hand (hand : string list) =
  let bid = List.nth hand 1 |> int_of_string in
  let hand = List.nth hand 0 in
  let hand = String.to_seq hand |> List.of_seq in
  let hand = List.map map hand in
  let counts = count_chars hand in
  match hand with
  | [ a; b; c; d; e ] when a = b && b = c && c = d && d = e ->
      { hand_type = FiveOfAKind; hand; bid }
  | _ when max_count counts = 4 -> { hand_type = FourOfAKind; hand; bid }
  | _ when max_count counts = 3 && has_one_pair counts ->
      { hand_type = FullHouse; hand; bid }
  | _ when max_count counts = 3 -> { hand_type = ThreeOfAKind; hand; bid }
  | _ when is_two_pairs counts -> { hand_type = TwoPairs; hand; bid }
  | _ when max_count counts = 2 -> { hand_type = OnePair; hand; bid }
  | _ -> { hand_type = HighCard; hand; bid }

let parse (lines : string list) =
  let lines = List.map (String.split_on_char ' ') lines in
  let hands = List.map string_to_hand lines in
  hands

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "6440";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
