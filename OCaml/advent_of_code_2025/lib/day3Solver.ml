type joltage = int
type battery = joltage
type bank = battery list
type input_t = bank list


let parse_bank s = let re = Re.Perl.re "^\\d+$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> 
    (Re.Group.get groups 0) |> 
      String.to_seq |> 
      Seq.map (fun c -> c |> Char.escaped |> int_of_string) |>
      List.of_seq) 
    maybe_groups
 
let parse s = let lines = String.split_on_char '\n' s in
  let maybe_banks = List.filter (fun s ->  not (String.length s = 0)) lines |>
  (List.map parse_bank) in
  List.fold_right (fun maybe_bank acc -> match maybe_bank, acc with
    None, _ -> None
    | _, None -> None
    | Some bank, Some banks -> Some (bank :: banks))
    maybe_banks
    (Some [])

let max_pair_joltage bank = let (first, second) = List.fold_left 
    (fun (left, right) next -> if right > left then (right, next) else if next > right then (left, next) else (left, right))
    (0, 0)
    bank
  in
  first * 10 + second

let solve_part_1 banks = List.fold_left (fun joltage bank -> joltage + max_pair_joltage bank) 0 banks |> string_of_int


let new_best_battery_sequence seq next = let (ordered_start_rev, last_if_not_found, found) = List.fold_left
  (fun (start, prior, found) next -> if found then (start, prior, true) else
    if next > prior then (start, prior, true) else 
      (prior :: start, next, false))
  ([], (List.hd seq), false)
  (List.drop 1 seq) in
  if not found then
      if next > last_if_not_found then 
        (next :: ordered_start_rev) |> List.rev 
      else seq
  else
    let ordered_start = List.rev ordered_start_rev in
    let remainder = List.drop ((List.length ordered_start) + 1) seq in
    List.concat [ordered_start; remainder; [next]]

let max_sequence_joltage len bank = if len <= 0 then 0 else
  let seq = List.fold_left 
    new_best_battery_sequence
    (Seq.repeat 0 |> Seq.take @@ len |> List.of_seq)
    bank
  in
  List.fold_left
    (fun acc j -> 10 * acc + j)
    0
    seq

let solve_part_2 banks = List.fold_left (fun joltage bank -> joltage + max_sequence_joltage 12 bank) 0 banks |> string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "987654321111111
811111111111119
234234234234278
818181911112111
"


let%test "parses example" = let expectation = [
    [9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1];
    [8;1;1;1;1;1;1;1;1;1;1;1;1;1;9];
    [2;3;4;2;3;4;2;3;4;2;3;4;2;7;8];
    [8;1;8;1;8;1;9;1;1;1;1;2;1;1;1]
  ] in
  let actual = parse example_input  in
  Option.equal (List.equal @@ (fun a b -> a = b)) actual (Some expectation)


let%test "part 1 works for example" = let expectation = "357" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 3 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "17445" in
  let input_text = InputReader.read_day_input 3 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 3 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "3121910778619" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 3 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "173229689350551" in
  let input_text = InputReader.read_day_input 3 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 3 on real inputs is " ^ actual);
  actual = expectation
