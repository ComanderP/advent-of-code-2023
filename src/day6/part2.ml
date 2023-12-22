open Utils

let day = D6

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let winning_scores time distance : int list =
  let rec aux i record_passed (acc : int list) : int list =
    let travelled = i * (time - i) in
    if record_passed then
      if travelled < distance then acc
      else aux (i + 1) record_passed (travelled :: acc)
    else aux (i + 1) (travelled > distance) (travelled :: acc)
  in
  aux 0 false []

let get_total_valid_wins distance (scores : int list) : int =
  List.fold_left (fun acc x -> if x > distance then acc + 1 else acc) 0 scores

let solve (input : int list list) : string =
  let times = List.hd input in
  let distances = List.nth input 1 in
  List.map2 winning_scores times distances
  |> List.map2 get_total_valid_wins distances
  |> List.fold_left ( * ) 1 |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) =
  let open List in
  let lines = map (String.split_on_char ' ') lines in
  let lines = map (filter_map int_of_string_opt) lines in
  let lines = map (map string_of_int) lines in
  let lines = map (String.concat "") lines in
  let lines = map (fun x -> [ int_of_string x ]) lines in
  lines

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "71503";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
