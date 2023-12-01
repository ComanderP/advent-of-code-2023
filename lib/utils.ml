module In_channel = Core.In_channel

type day =
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  | D10
  | D11
  | D12
  | D13
  | D14
  | D15
  | D16
  | D17
  | D18
  | D19
  | D20
  | D21
  | D22
  | D23
  | D24
  | D25

let string_of_day = function
  | D1 -> "1"
  | D2 -> "2"
  | D3 -> "3"
  | D4 -> "4"
  | D5 -> "5"
  | D6 -> "6"
  | D7 -> "7"
  | D8 -> "8"
  | D9 -> "9"
  | D10 -> "10"
  | D11 -> "11"
  | D12 -> "12"
  | D13 -> "13"
  | D14 -> "14"
  | D15 -> "15"
  | D16 -> "16"
  | D17 -> "17"
  | D18 -> "18"
  | D19 -> "19"
  | D20 -> "20"
  | D21 -> "21"
  | D22 -> "22"
  | D23 -> "23"
  | D24 -> "24"
  | D25 -> "25"

type file = Input | Example1 | Example2

let string_of_file = function
  | Input -> "input"
  | Example1 -> "ex1"
  | Example2 -> "ex2"

let get_input day file =
  let input_file =
    "data/day" ^ string_of_day day ^ "/" ^ string_of_file file ^ ".txt"
  in
  In_channel.read_lines input_file

let test1 day parse solve expected =
  let solution = get_input day Example1 |> parse |> solve in
  if solution <> expected then
    Printf.printf "Test failed, expected %s, got %s\n" expected solution
  else Printf.printf "Test passed, got %s\n" solution

let test2 day parse solve expected =
  let solution = get_input day Example2 |> parse |> solve in
  if solution <> expected then
    Printf.printf "Test failed, expected %s, got %s\n" expected solution
  else Printf.printf "Test passed, got %s\n" solution
