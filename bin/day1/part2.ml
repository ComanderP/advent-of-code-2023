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

(** Convert a list of chars to a string *)
let implode l = String.of_seq (List.to_seq l)

let assoc =
  [
    ("one", '1');
    ("two", '2');
    ("three", '3');
    ("four", '4');
    ("five", '5');
    ("six", '6');
    ("seven", '7');
    ("eight", '8');
    ("nine", '9');
    ("zero", '0');
  ]

let prefixes =
  [
    "one";
    "two";
    "three";
    "four";
    "five";
    "six";
    "seven";
    "eight";
    "nine";
    "zero";
  ]

(** Get the numbers in a line *)
let get_numbers line =
  let rec get_numbers' line acc =
    match line with
    | [] -> acc
    | hd :: tl ->
        (* If digit, add to acc *)
        if is_digit hd then get_numbers' tl (hd :: acc)
          (* Else, check if it's a prefix *)
        else
          let line_str = implode line in
          let rec find_prefixes prefixes acc =
            match prefixes with
            | [] -> get_numbers' tl acc
            | prefix :: rest ->
                if String.starts_with ~prefix line_str then
                  get_numbers' tl (List.assoc prefix assoc :: acc)
                else find_prefixes rest acc
          in
          find_prefixes prefixes acc
  in
  List.rev (get_numbers' (explode line) [])

let parse (lines : string list) =
  let numbers = Array.make (List.length lines) "" in
  for i = 0 to List.length lines - 1 do
    let line = List.nth lines i in
    let numbers' = get_numbers line in
    let first = List.hd numbers' in
    let last = List.rev numbers' |> List.hd in
    numbers.(i) <- Char.escaped first ^ Char.escaped last
  done;
  Array.to_list numbers |> List.map int_of_string

(*****************************************************************************)
(*                             NO IMPERATIVE                                 *)
(*****************************************************************************)

let parse' (lines : string list) =
  let numbers = List.map get_numbers lines in
  List.map
    (fun numbers' ->
      let first = List.hd numbers' in
      let last = List.rev numbers' |> List.hd in
      int_of_string @@ Char.escaped first ^ Char.escaped last)
    numbers

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "281";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
