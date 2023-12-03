open Utils

let day = D3

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let get_number (line : char array) (j : int) : int * int list =
  let rec aux j acc =
    if j < 0 || j >= Array.length line || not (is_digit line.(j)) then
      (int_of_string (fst acc), snd acc)
    else aux (j + 1) (fst acc ^ Char.escaped line.(j), snd acc @ [ j ])
  in
  aux j ("", [])

let adjacent i j matrix =
  let rec aux i j stop =
    match matrix.(i).(j) with
    | c when (not (is_digit c)) && c <> '.' -> true
    | _ when stop -> false
    | _ ->
        aux (i + 1) j true
        || aux (i - 1) j true
        || aux i (j + 1) true
        || aux i (j - 1) true
        || aux (i + 1) (j + 1) true
        || aux (i + 1) (j - 1) true
        || aux (i - 1) (j + 1) true
        || aux (i - 1) (j - 1) true
  in
  aux i j false

let solve (input : char array array) : string =
  let was_processed =
    Array.make_matrix (Array.length input) (Array.length input.(0)) false
  in
  let answer = ref 0 in
  Array.iteri
    (fun i line ->
      Array.iteri
        (fun j char ->
          if is_digit char && not was_processed.(i).(j) then
            let n, l = get_number line j in
            let stop = ref false in
            List.iter
              (fun j ->
                was_processed.(i).(j) <- true;
                if !stop then ()
                else if adjacent i j input then (
                  answer := !answer + n;
                  stop := true))
              l)
        line)
    input;
  string_of_int !answer

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) =
  let grid =
    Array.make_matrix
      (List.length lines + 2)
      (String.length (List.hd lines) + 2)
      '.'
  in
  List.iteri
    (fun i line ->
      String.iteri (fun j character -> grid.(i + 1).(j + 1) <- character) line)
    lines;
  grid

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "4361";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
