open Utils

let day = D2

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let minimum_amounts game : int * int * int =
  let rec aux r g b = function
    | [] -> (r, g, b)
    | (number, colour) :: tl -> (
        match colour with
        | "red" -> aux (max number r) g b tl
        | "green" -> aux r (max number g) b tl
        | "blue" -> aux r g (max number b) tl
        | _ -> failwith ("Invalid colour: " ^ colour))
  in
  aux 0 0 0 game

let solve input : string =
  List.map (fun game -> minimum_amounts game) input
  |> List.map (fun (r, g, b) -> r * g * b)
  |> List.fold_left ( + ) 0 |> string_of_int

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
  test2 day parse solve "2286";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
