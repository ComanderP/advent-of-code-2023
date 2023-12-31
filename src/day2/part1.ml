open Utils
open List

let day = D2

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let is_possible game r g b : bool =
  let rec aux = function
    | [] -> true
    | (number, colour) :: tl -> (
        match colour with
        | "red" -> r >= number && aux tl
        | "green" -> g >= number && aux tl
        | "blue" -> b >= number && aux tl
        | _ -> failwith ("Invalid colour: " ^ colour))
  in
  aux game

let solve input : string =
  let answer =
    map (fun game -> is_possible game 12 13 14) input
    |> mapi (fun i b -> if b then i + 1 else 0)
  in
  fold_left ( + ) 0 answer |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let group line : (int * string) list =
  let rec aux acc = function
    | n :: c :: tl -> aux ((int_of_string n, c) :: acc) tl
    | _ -> acc
  in
  rev (aux [] line)

let parse (lines : string list) =
  map (String.split_on_char ':') lines
  |> map (fun x -> rev x |> hd)
  |> map (split_on_chars [ ' '; ','; ';' ])
  |> map (filter (fun s -> s <> ""))
  |> map group

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "8";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
