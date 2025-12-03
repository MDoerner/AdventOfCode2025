(*
type day = Day1 | Day2 | Day3 | Day4 | Day5 | Day6 | Day7 | Day8 | Day9 | Day10 | Day11 | Day12

let int_to_day n = 
  match n with
    1 -> Day1
  | 2 -> Day2
  | 3 -> Day3
  | 4 -> Day4
  | 5 -> Day5
  | 6 -> Day6
  | 7 -> Day7
  | 8 -> Day8
  | 9 -> Day9
  | 10 -> Day10
  | 11 -> Day11
  | 12 -> Day12
  | _ -> failwith "Days of AdventOfCode must be between 1 and 12."

let int_to_part n =
  match n with
    1 -> Advent_of_code_2025.DaySolver.Part1
  | 2 -> Advent_of_code_2025.DaySolver.Part2
  | _ -> failwith "Parts of AdventOfCode must be either 1 or 2." 

let day_solver_selector _day = 
   match day with
    Day1 -> Advent_of_code_2025.DaySolver.Make(Day1Solver)
  |  _ -> failwith "Selected day not yet implemented."

let solve_part_of_day day_number part_number = 
  let part = int_to_part part_number in
  let day = int_to_day day_number in
  let solver = day_solver_selector day in
  let input = Advent_of_code_2025.InputReader.read_day_input day_number in
  let result = solver.solve_part part input in
  match result with 
    Some s -> s
  |  None -> failwith "Unable to parse the days's input string to the expected input type."

let () = let day_number = Sys.argv(1) |> int_of_string in 
  let part_number = Sys.argv(2) |> int_of_string in 
  solve_part_of_day day_number part_number |>
  print_endline *)

let () = print_endline "not done yet"