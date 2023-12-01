open Utils

let day = D2

(* Solution *)
let solve input : string = assert false

(* Input parsing *)
let parse (lines : string list) = assert false

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "TODO";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
