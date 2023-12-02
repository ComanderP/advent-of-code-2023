open Utils

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
    List.map (fun game -> is_possible game 12 13 14) input
    |> List.mapi (fun i b -> if b then i + 1 else 0)
  in
  List.fold_left ( + ) 0 answer |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let group line : (int * string) list =
  let rec aux acc = function
    | n :: c :: tl -> aux ((int_of_string n, c) :: acc) tl
    | _ -> acc
  in
  List.rev (aux [] line)

let parse (lines : string list) =
  List.map (String.split_on_char ':') lines
  |> List.map (fun x -> List.rev x |> List.hd)
  |> List.map (Core.String.split_on_chars ~on:[ ' '; ','; ';' ])
  |> List.map (List.filter (fun s -> s <> ""))
  |> List.map group

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "8";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
