open Utils

let day = D8

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let rec follow instructions nodes current acc =
  if String.ends_with ~suffix:"Z" current then acc
  else
    let f =
      match instructions () with
      | Some 'L' -> fst
      | Some 'R' -> snd
      | _ -> failwith "Unreachable"
    in
    let dest = f (Hashtbl.find nodes current) in
    follow instructions nodes dest (acc + 1)

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let solve input : string =
  let instructions, nodes = input in
  let starting_nodes =
    Hashtbl.to_seq_keys nodes |> Seq.filter (String.ends_with ~suffix:"A")
  in
  let m =
    Seq.map
      (fun x -> follow (Seq.to_dispenser instructions) nodes x 0)
      starting_nodes
  in
  let m = Seq.fold_left lcm 1 m in
  string_of_int m

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
  test2 day parse solve "6";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
