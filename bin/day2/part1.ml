open Utils

let day = D1

(* Solution *)
let solve input = assert false

(* Input parsing *)
let parse lines = assert false

(* Main function to read input and run the solution *)
let () =
  test day parse solve "TODO";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
