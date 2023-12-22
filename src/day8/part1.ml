open Utils

let day = D8

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec follow instructions nodes current acc =
  if current = "ZZZ" then acc
  else
    let f =
      match instructions () with
      | Some 'L' -> fst
      | Some 'R' -> snd
      | _ -> failwith "Unreachable"
    in
    let dest = f (Hashtbl.find nodes current) in
    follow instructions nodes dest (acc + 1)

let solve input : string =
  let instructions, nodes = input in
  follow (Seq.to_dispenser instructions) nodes "AAA" 0 |> string_of_int

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let node_of_string line =
  Scanf.sscanf line "%s = (%s@, %s@)" (fun a b c -> (a, (b, c)))

let parse (lines : string list) =
  let instructions = String.to_seq (List.hd lines) |> Seq.cycle in
  let nodes = List.map node_of_string (List.tl (List.tl lines)) in
  let nodes = List.to_seq nodes |> Hashtbl.of_seq in
  (instructions, nodes)

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "6";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
