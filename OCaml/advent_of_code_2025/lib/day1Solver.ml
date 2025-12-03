type direction = Left | Right
type movement = {direction : direction; amount : int}
type input_t = movement list
  
let direction_from_string s = match s with
    "R" -> Right
  | "L" -> Left
  | _ -> failwith "Only R and L specify directions."

let parse_movement s = let re = Re.Perl.re "^(R|L)(\\d+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> {direction = (Re.Group.get groups 1) |> direction_from_string; amount = (Re.Group.get groups 2) |> int_of_string }) maybe_groups


 
let parse s = let lines = String.split_on_char '\n' s in
  let maybe_moves = List.filter (fun s ->  not (String.length s = 0)) lines |>
  (List.map parse_movement) in
  List.fold_right (fun maybe_move acc -> match maybe_move, acc with
    None, _ -> None
    | _, None -> None
    | Some move, Some moves -> Some (move :: moves))
    maybe_moves
    (Some [])

type position = int

let move (pos : position) {direction : direction; amount : int} = let unbounded_pos = match direction with 
  Left -> pos - amount
  | Right -> pos + amount
in (unbounded_pos mod 100 + 100) mod 100

let zero_hits start_pos movements = let (_last_pos, hits) =  List.fold_left 
  (fun (current_pos, hits) next_move -> 
    let new_pos = move current_pos next_move in
    let new_hits = if new_pos = 0 then hits + 1 else hits in
    (new_pos, new_hits))
    (start_pos, 0)
    movements in
    hits

let move_with_zero_detection (pos : position) {direction : direction; amount : int} = let unbounded_pos = match direction with 
  Left -> pos - amount
  | Right -> pos + amount
in let new_pos = (unbounded_pos mod 100 + 100) mod 100 in
let passes = match direction with 
  Left -> (amount + (100 - pos) mod 100) / 100
  | Right -> (pos + amount) / 100 in 
(new_pos, passes)

let zero_passes start_pos movements = let (_last_pos, passes) =  List.fold_left 
  (fun (current_pos, passes) next_move -> 
    let (new_pos, additional_passes) = move_with_zero_detection current_pos next_move in
    let new_passes = passes + additional_passes in
    (new_pos, new_passes))
    (start_pos, 0)
    movements in
    passes

let solve_part_1 moves = moves |>
    zero_hits @@ 50 |>
    string_of_int

let solve_part_2 moves = moves |>
    zero_passes @@ 50 |>
    string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
"

let%test "part 1 parses example" = let expectation = [
  {direction = Left; amount = 68};
  {direction = Left; amount = 30};
  {direction = Right; amount = 48};
  {direction = Left; amount = 5};
  {direction = Right; amount = 60};
  {direction = Left; amount = 55};
  {direction = Left; amount = 1};
  {direction = Left; amount = 99};
  {direction = Right; amount = 14};
  {direction = Left; amount = 82}
  ] in
  let actual = parse example_input  in
  Option.equal (List.equal @@ (fun a b -> a = b)) actual (Some expectation)


let%test "part 1 works for example" = let expectation = "3" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 1 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "980" in
  let input_text = InputReader.read_day_input 1 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 1 on real inputs is " ^ actual);
  actual = expectation



let%test "part 2 works for example" = let expectation = "6" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 1 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "5961" in
  let input_text = InputReader.read_day_input 1 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 1 on real inputs is " ^ actual);
  actual = expectation
