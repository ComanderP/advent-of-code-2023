open Utils

let day = D5

type range_map = { source : int; destination : int; length : int }
type map = range_map list
type range = int * int
type almanac = { seeds : range list; maps : map list }

(*****************************************************************************)
(*                                SOLUTION                                   *)
(*****************************************************************************)

let intersections ((s, len) : range) range_map : range list =
  let { source = s'; length; _ } = range_map in
  let e = s + len and e' = s' + length in
  let mk rs re = (rs, re - rs) in
  match (s, e, s', e') with
  | _ when s > e' -> [ (s, len) ]
  | _ when e < s' -> [ (s, len) ]
  | _ when s < s' ->
      mk s (s' - 1)
      :: (if e <= e' then [ mk s' e ] else [ mk s' e'; mk (e' + 1) e ])
  | _ when s <= e' -> if e <= e' then [ mk s e ] else [ mk s e'; mk (e' + 1) e ]
  | _ -> failwith "unreachable"

let rec transform_range range range_maps : range list =
  match range_maps with
  | [] -> [ range ]
  | range_map :: tl ->
      let apply range_map range =
        let start, len = range in
        let { source; destination; _ } = range_map in
        (start - source + destination, len)
      in
      let within range_map i =
        let { source; length; _ } = range_map in
        i >= source && i <= source + length
      in
      let transform x =
        if within range_map (fst x) then [ apply range_map x ]
        else transform_range x tl
      in
      List.concat_map transform (intersections range range_map)

let transform_ranges ranges maps : range list =
  List.concat_map (fun range -> transform_range range maps) ranges

let solve (input : almanac) : string =
  let sol = List.fold_left transform_ranges input.seeds input.maps in
  let minimum = List.fold_left (fun acc (x, _) -> min acc x) max_int sol in
  string_of_int minimum

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

let get_pairs seeds : range list =
  let rec aux acc = function
    | a :: b :: tl -> aux ((a, b) :: acc) tl
    | _ -> List.rev acc
  in
  aux [] seeds

let parse (lines : string list) : almanac =
  let seeds = lines |> seeds |> get_pairs in
  let maps = get_maps List.(lines |> tl |> tl) in
  { seeds; maps }

(* Main function to read input and run the solution *)
let () =
  test2 day parse solve "46";
  let solution = get_input day Input |> parse |> solve in
  print_endline solution
