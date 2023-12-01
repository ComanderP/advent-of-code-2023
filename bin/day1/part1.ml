open Utils

let day = D1

(* Solution *)
let solve (input : int list) = List.fold_left ( + ) 0 input |> string_of_int

(* Input parsing *)
let parse (lines : string list) : int list =
  let open Core in
  let numbers = Array.create ~len:(List.length lines) "" in
  for i = 0 to List.length lines - 1 do
    let line = List.nth lines i |> Option.value_exn in
    let first =
      List.find ~f:(fun c -> Char.is_digit c) (String.to_list line)
      |> Option.value_exn
    and last =
      List.find ~f:(fun c -> Char.is_digit c) (List.rev (String.to_list line))
      |> Option.value_exn
    in
    numbers.(i) <- String.of_char first ^ String.of_char last
  done;
  Array.to_list numbers |> List.map ~f:int_of_string

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "142";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
