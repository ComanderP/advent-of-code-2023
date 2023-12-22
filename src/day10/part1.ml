open Utils

let day = D10

type maze = char array array

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let find_start (maze : maze) =
  let h = Array.length maze in
  let w = Array.length maze.(0) in
  let rec aux i j =
    if maze.(i).(j) = 'S' then (i, j)
    else if j = w - 1 then if i = h - 1 then raise Not_found else aux (i + 1) 0
    else aux i (j + 1)
  in
  aux 0 0

let solve (input : maze) : string =
  let x = find_start input in
  Printf.printf "%d %d" (fst x) (snd x);
  ""

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let parse (lines : string list) : maze =
  List.map (fun x -> x |> String.to_seq |> Array.of_seq) lines |> Array.of_list

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "8";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
