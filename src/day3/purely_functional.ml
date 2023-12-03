open Utils

module Set = Set.Make (struct
  type t = int * int

  let compare = compare
end)

module Map = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let pos (i, j) =
  [ (i + 1, j); (i - 1, j); (i, j + 1); (i, j - 1); (i + 1, j + 1); (i + 1, j - 1); (i - 1, j + 1); (i - 1, j - 1) ]

let reverse s = String.(init (length s) (fun i -> s.[length s - i - 1]))

let line_recurser (y, acc) =
  let rec go ((x, y), c, ns, res) = function
    | [] -> if c = "" then (y + 1, res) else (y + 1, (int_of_string (reverse c), ns) :: res)
    | h :: t when (not (is_digit h)) && c = "" -> go ((x + 1, y), c, Set.empty, res) t
    | h :: t when not (is_digit h) -> go ((x + 1, y), "", Set.empty, (int_of_string (reverse c), ns) :: res) t
    | h :: t -> go ((x + 1, y), Char.escaped h ^ c, Set.union ns (Set.of_list (pos (x, y))), res) t
  in
  go ((0, y), "", Set.empty, acc)

let numbers_and_nbrs x = snd (List.fold_left line_recurser (0, []) x)
let sum = List.fold_left (fun acc (x, _) -> acc + x) 0
let explode s = List.init (String.length s) (String.get s)
let not_dig_or_dot c = not (is_digit c || c = '.')
let folder f ((_, y), s) = String.fold_left f ((0, y + 1), s)
let get_coords f input = snd (List.fold_left (folder f) ((0, -1), Set.empty) input)
let checker f ((x, y), s) (ch : char) = if f ch then ((x + 1, y), Set.add (x, y) s) else ((x + 1, y), s)

let part1 input =
  let syms = get_coords (checker not_dig_or_dot) input in
  let f (_, s) = not (Set.is_empty (Set.inter s syms)) in
  List.map explode input |> numbers_and_nbrs |> List.filter f |> sum |> string_of_int

let single_tally (num, s) mp =
  let func pt acc = Map.add pt (num :: Option.value ~default:[] (Map.find_opt pt acc)) acc in
  Set.fold func s mp

let tally_nbrs x = List.fold_right single_tally x Map.empty
let product = List.fold_left ( * ) 1

let part2 input =
  let tally = tally_nbrs (numbers_and_nbrs (List.map explode input)) in
  let gears = get_coords (checker (( = ) '*')) input in
  let f pos acc =
    let l = Option.value ~default:[] (Map.find_opt pos tally) in
    if List.length l = 2 then acc + product l else acc
  in
  Set.fold f gears 0 |> string_of_int

let () =
  test1 D3 part1 (fun x -> x) "4361";
  get_input D3 Input |> part1 |> print_endline;
  test2 D3 part2 (fun x -> x) "467835";
  get_input D3 Input |> part2 |> print_endline
