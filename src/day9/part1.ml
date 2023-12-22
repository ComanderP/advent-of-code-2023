open Utils

let day = D9

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let[@tail_mod_cons] rec seq_diff seq : int list =
  match seq with
  | x1 :: x2 :: tail -> (x2 - x1) :: seq_diff (x2 :: tail)
  | _ -> []

let sequences seq =
  let rec aux acc = function
    | seq when List.for_all (( = ) 0) seq -> seq :: acc
    | seq -> aux (seq :: acc) (seq_diff seq)
  in
  aux [] seq

let solve (input : int list list) : string =
  input |> List.map sequences
  |> List.map (List.map List.rev)
  |> List.map (List.fold_left (fun acc x -> acc + List.hd x) 0)
  |> List.fold_left ( + ) 0 |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) : int list list =
  List.map
    (fun line -> line |> String.split_on_char ' ' |> List.map int_of_string)
    lines

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "114";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
