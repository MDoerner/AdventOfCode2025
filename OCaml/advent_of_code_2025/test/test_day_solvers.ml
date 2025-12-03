(*open Advent_of_code_2025

let _ = let expectation = "3" in
  let input_text = InputReader.read_day_input 1 in
  let input = Day1Solver.parse input_text |> Option.get in
  let actual = Day1Solver.solve_part DaySolver.Part1 input in
  print_endline ("Computed solution is " ^ actual);
  if actual != expectation then exit 1;
*)