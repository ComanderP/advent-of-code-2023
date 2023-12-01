open Utils

let day = D1

(* Solution *)
let solve input : string = List.fold_left ( + ) 0 input |> string_of_int

let explode s =
  let rec expl i l = if i < 0 then l else expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let get_numbers line =
  let open Core in
  let rec get_numbers' line' acc i =
    match line' with
    | [] -> acc
    | hd :: tl ->
        if Char.is_digit hd then get_numbers' tl (hd :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"one" then
          get_numbers' tl (Char.of_string "1" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"two" then
          get_numbers' tl (Char.of_string "2" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"three" then
          get_numbers' tl (Char.of_string "3" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"four" then
          get_numbers' tl (Char.of_string "4" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"five" then
          get_numbers' tl (Char.of_string "5" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"six" then
          get_numbers' tl (Char.of_string "6" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"seven" then
          get_numbers' tl (Char.of_string "7" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"eight" then
          get_numbers' tl (Char.of_string "8" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"nine" then
          get_numbers' tl (Char.of_string "9" :: acc) (i + 1)
        else if String.is_substring_at line ~pos:i ~substring:"zero" then
          get_numbers' tl (Char.of_string "0" :: acc) (i + 1)
        else get_numbers' tl acc (i + 1)
  in
  List.rev (get_numbers' (explode line) [] 0)

(* Input parsing *)
let parse (lines : string list) =
  let open Core in
  let numbers = Array.create ~len:(List.length lines) "" in
  for i = 0 to List.length lines - 1 do
    let line = List.nth lines i |> Option.value_exn in

    let numbers' = get_numbers line in

    let first = List.hd_exn numbers' in
    let last = List.last_exn numbers' in
    numbers.(i) <- Char.to_string first ^ Char.to_string last
  done;
  Array.to_list numbers |> List.map ~f:int_of_string

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "281";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
