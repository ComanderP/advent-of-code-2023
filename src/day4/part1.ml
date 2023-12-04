open Utils

let day = D4

type card = { winners : int list; numbers : int list }

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let solve (input : card list) : string =
  List.(
    map
      (fun card ->
        fold_left
          (fun acc x ->
            if mem x card.winners then if acc = 0 then 1 else acc * 2 else acc)
          0 card.numbers)
      input
    |> List.fold_left ( + ) 0 |> string_of_int)

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
  test1 day parse solve "13";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
