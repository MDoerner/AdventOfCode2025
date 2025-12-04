module Point = struct
  type t = int * int
  let compare (x1, y1) (x2, y2) = match compare x1 x2 with
    0 -> compare y1 y2
  | c -> c
end

module PointSet = Set.Make(Point)

type input_t = PointSet.t

let parse s = let lines = String.split_on_char '\n' s in
  if List.length lines = 0 then Some PointSet.empty else
    let (_, rolls) = List.fold_left
      (fun (y, occupied_points) line -> 
        let (_, new_occupied) = Seq.fold_left
          (fun (x, current_occupied) tile -> if not (tile = '@') then (x + 1, current_occupied) else
            (x + 1, PointSet.add (x, y) current_occupied))
          (0, occupied_points)
          (String.to_seq line)
        in
        (y + 1, new_occupied))
      (0, PointSet.empty)
      lines
    in 
    Some rolls

  

let neighbours point = let (x, y) = point in
    [(x-1,y-1); (x, y-1); (x+1, y-1); (x+1, y); (x+1, y+1); (x, y+1); (x-1, y+1); (x-1, y)]

let movable occupied_points point = let n_occupied_neighbours = 
  point |> neighbours |> List.filter (fun p -> PointSet.mem p occupied_points) |> List.length in
  n_occupied_neighbours < 4


let solve_part_1 rolls = rolls |> 
  PointSet.to_seq |>
  Seq.filter (movable @@ rolls) |>
  Seq.length |>
  string_of_int 


let remove_movable rolls = Seq.fold_left
  (fun remaining_rolls roll -> if movable rolls roll then PointSet.remove roll remaining_rolls else remaining_rolls)
  rolls
  (PointSet.to_seq rolls)

let rec remove_movable_rec rolls = let n_rolls = PointSet.cardinal rolls in
  let once_removed = remove_movable rolls in
  let new_n_rolls = PointSet.cardinal once_removed in
  (*let () = "old count = " ^ string_of_int n_rolls  ^ " | new count = " ^ string_of_int new_n_rolls |> print_endline in*)
  if new_n_rolls = n_rolls then 
    rolls 
  else
    remove_movable_rec once_removed


let solve_part_2 rolls = let n_rolls = PointSet.cardinal rolls in
    let immovable = remove_movable_rec rolls in
    let n_immovable = PointSet.cardinal immovable in
    let n_removable = n_rolls - n_immovable in
    string_of_int n_removable

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"



let%test "part 1 works for example" = let expectation = "13" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 4 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "1578" in
  let input_text = InputReader.read_day_input 4 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 4 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "43" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 4 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "10132" in
  let input_text = InputReader.read_day_input 4 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 4 on real inputs is " ^ actual);
  actual = expectation
