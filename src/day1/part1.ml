open Utils

let day = D1

(*****************************************************************************)
(*                               SOLUTION                                    *)
(*****************************************************************************)

let solve input : string = List.fold_left ( + ) 0 input |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

(** Check if a char is a digit *)
let is_digit (c : char) : bool = match c with '0' .. '9' -> true | _ -> false

(** Convert a string to a list of chars *)
let explode s = List.init (String.length s) (fun i -> s.[i])

let parse (lines : string list) : int list =
  let numbers = Array.make (List.length lines) "" in
  for i = 0 to List.length lines - 1 do
    let line = List.nth lines i in
    let first = List.find (fun c -> is_digit c) (explode line)
    and last = List.find (fun c -> is_digit c) (List.rev (explode line)) in
    numbers.(i) <- String.make 1 first ^ String.make 1 last
  done;
  Array.to_list numbers |> List.map int_of_string

(*****************************************************************************)
(*                             NO IMPERATIVE                                 *)
(*****************************************************************************)

let parse' (lines : string list) : int list =
  List.map
    (fun line ->
      let first = List.find (fun c -> is_digit c) (explode line) in
      let last = List.find (fun c -> is_digit c) (List.rev (explode line)) in
      int_of_string @@ Char.escaped first ^ Char.escaped last)
    lines

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "142";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
