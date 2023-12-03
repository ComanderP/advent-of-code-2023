open Utils

let day = D3

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let pos (i, j) =
  [
    (i + 1, j);
    (i - 1, j);
    (i, j + 1);
    (i, j - 1);
    (i + 1, j + 1);
    (i + 1, j - 1);
    (i - 1, j + 1);
    (i - 1, j - 1);
  ]

let get_number (line : char array) (j : int) : int * int list =
  let rec aux j acc =
    if j < 0 || j >= Array.length line || not (is_digit line.(j)) then
      (int_of_string (fst acc), snd acc)
    else aux (j + 1) (fst acc ^ Char.escaped line.(j), snd acc @ [ j ])
  in
  aux j ("", [])

let adjacent i j matrix =
  let rec aux (i, j) stop =
    match matrix.(i).(j) with
    | '*' -> true
    | _ when stop -> false
    | _ ->
        List.fold_left
          (fun acc (i, j) -> acc || aux (i, j) true)
          false
          (pos (i, j))
  in
  aux (i, j) false

let get_star_pos i j matrix =
  let rec aux (i, j) coords =
    match matrix.(i).(j) with
    | '*' -> (i, j)
    | _ -> aux (List.hd coords) (List.tl coords)
  in
  aux (i, j) (pos (i, j))

let solve (input : char array array) : string =
  let was_processed =
    Array.make_matrix (Array.length input) (Array.length input.(0)) false
  in
  let stars = ref [] in
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
                  let star_pos = get_star_pos i j input in
                  (match List.assoc_opt star_pos !stars with
                  | Some l -> l := !l @ [ n ]
                  | None -> stars := (star_pos, ref [ n ]) :: !stars);
                  stop := true))
              l)
        line)
    input;
  let stars = List.filter (fun (_, l) -> List.length !l = 2) !stars in
  List.fold_left (fun acc (_, l) -> acc + List.fold_left ( * ) 1 !l) 0 stars
  |> string_of_int

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
  test2 day parse solve "467835";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
