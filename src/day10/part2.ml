open Utils

let day = D10

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)
let solve input : string = assert false

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)
let parse (lines : string list) = assert false

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "TODO";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
