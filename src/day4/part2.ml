open Utils

let day = D4

type card = { winners : int list; numbers : int list }

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec winnings (card : card) =
  match card with
  | { winners; numbers = hd :: tl } ->
      if List.mem hd winners then 1 + winnings { winners; numbers = tl }
      else winnings { winners; numbers = tl }
  | _ -> 0

let solve (input : card list) : string =
  let total_cards = Array.make (List.length input) 1 in
  let wins = List.map winnings input in
  List.iteri
    (fun i win ->
      for _ = 1 to total_cards.(i) do
        let rec loop k =
          if k <= win then (
            total_cards.(i + k) <- total_cards.(i + k) + 1;
            loop (k + 1))
        in
        loop 1
      done)
    wins;
  Array.fold_left (fun acc x -> acc + x) 0 total_cards |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) =
  List.(
    lines
    |> map (split_on_chars [ ':'; '|' ])
    |> map tl
    |> map (map String.trim)
    |> map (map (String.split_on_char ' '))
    |> map (map (filter_map int_of_string_opt))
    |> map (fun x -> { winners = hd x; numbers = nth x 1 }))

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "30";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
