open Utils

let day = D5

type range_map = { source : int; destination : int; length : int }
type map = range_map list
type almanac = { seeds : int list; maps : map list }

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let follow_map (almanac : almanac) : almanac =
  let rec follow seeds acc : int list =
    match seeds with
    | [] -> acc
    | seed :: rest -> (
        match
          List.find_opt
            (fun { source; length; _ } ->
              seed >= source && seed < source + length)
            (List.hd almanac.maps)
        with
        | None -> follow rest (seed :: acc)
        | Some { source; destination; _ } ->
            let diff = destination - source in
            let mapped = seed + diff in
            follow rest (mapped :: acc))
  in
  { seeds = follow almanac.seeds []; maps = List.tl almanac.maps }

let rec follow_maps (almanac : almanac) : almanac =
  if List.length almanac.maps = 0 then almanac
  else follow_maps (follow_map almanac)

let solve (input : almanac) : string =
  let almanac = follow_maps input in
  let min = List.fold_left min max_int almanac.seeds in
  string_of_int min

(*****************************************************************************)
(*                            INPUT PROCESSING                               *)
(*****************************************************************************)

let lines_to_map (lines : string list) : map * string list =
  let rec aux lines acc =
    match lines with
    | [] -> (acc, [])
    | "" :: tl -> (acc, tl)
    | line :: tl ->
        let range_map =
          Scanf.sscanf line "%d %d %d" (fun destination source length ->
              { destination; source; length })
        in
        aux tl (range_map :: acc)
  in
  aux (List.tl lines) []

let get_maps lines : map list =
  let rec aux lines acc =
    match lines with
    | [] -> List.rev acc
    | _ ->
        let map, lines = lines_to_map lines in
        aux lines (map :: acc)
  in
  aux lines []

let seeds lines =
  lines |> List.hd
  |> split_on_chars [ ':'; ' ' ]
  |> List.filter_map int_of_string_opt

let parse (lines : string list) : almanac =
  let seeds = seeds lines in
  let maps = get_maps List.(lines |> tl |> tl) in
  { seeds; maps }

(* Main function to read input and run the solution *)
let () =
  test1 day parse solve "35";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
