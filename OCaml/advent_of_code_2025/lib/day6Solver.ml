type operator = Plus | Mult
type operand = int
type computation = {operator : operator; operands : operand list} 
type input_t = operator list * string list


let parse_items_to_list parse_item item_texts = let maybe_items = item_texts |> 
  List.map @@ parse_item in
    List.fold_right (fun maybe_item acc -> match maybe_item, acc with
      None, _ -> None
      | _, None -> None
      | Some item, Some items -> Some (item :: items))
      maybe_items
      (Some [])

let parse_operator s = match s with
  "+" -> Some Plus
  | "*" -> Some Mult
  | _ -> None

let parse_operators s = let re = Re.Perl.re "\\s+" |> Re.compile in
  let operator_texts = s |> Re.split re |> List.filter (fun op_s ->  not (String.length op_s = 0)) in
  parse_items_to_list parse_operator operator_texts

let parse_operand s = let re = Re.Perl.re "^\\d+$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> (Re.Group.get groups 0) |> int_of_string) maybe_groups

let parse_operand_line s = let re = Re.Perl.re "\\s+" |> Re.compile in
  let operand_texts = s |> Re.split re |> List.filter (fun op_s ->  not (String.length op_s = 0)) in
  parse_items_to_list parse_operand operand_texts

let transpose ls = if ls = [] then [] else
  let max_len = List.fold_left (fun m l -> max m (List.length l)) 0 ls in
    if max_len = 0 then [] else
      let (transposed_rev, _) = Seq.fold_left (fun (transposed, rests) _ -> 
      let (next_transposed_rev, new_rests_rev) = List.fold_left (fun (transposed_start_rev, rests_start_rev) next_rest ->
        match next_rest with
        [] -> (transposed_start_rev, rests_start_rev)
        | (next :: rest_wo_next) -> (next :: transposed_start_rev, rest_wo_next :: rests_start_rev)
        ) ([],[]) rests in
        ((List.rev next_transposed_rev) :: transposed, List.rev new_rests_rev))
      ([], ls)
      (Seq.ints 0 |> Seq.take max_len) in
      List.rev transposed_rev


let parse_operands operand_lines = let maybe_row_wise_operands = parse_items_to_list parse_operand_line operand_lines in
  match maybe_row_wise_operands with
    None -> None
    | Some row_wise_operands -> Some (transpose row_wise_operands)




let parse s = let lines_rev = String.split_on_char '\n' s |> List.filter (fun s ->  not (String.length s = 0)) |> List.rev in
  match lines_rev with
  [] -> None
  | [_] -> None
  | (operator_line :: operand_lines_rev) -> let maybe_operators = parse_operators operator_line in
  match maybe_operators with
        None -> None
        | Some operators -> Some (operators, (List.rev operand_lines_rev))

let to_computations operands_parser (operators, operands_lines) = let maybe_operands = operands_parser operands_lines in
  match maybe_operands with
    None -> None
    | Some operand_lists -> if not (List.length operators = List.length operand_lists) then None else
      Some (List.map2 (fun operator operands -> {operator; operands}) operators operand_lists)


let add operands = List.fold_left (fun acc next -> acc + next) 0 operands

let mult operands = List.fold_left (fun acc next -> acc * next) 1 operands

let execute_computation computation = let {operator;operands} = computation in
  match operator with
    Plus -> add operands
    | Mult -> mult operands

let solve_part_1 input = let maybe_computations = to_computations parse_operands input in
  match maybe_computations with
  None -> failwith "Invalid input!"
  | Some computations -> computations |> List.fold_left (fun acc next -> acc + execute_computation next) 0 |> string_of_int


let transposed_lines ls = ls |> 
  List.map (fun s -> s |> String.to_seq |> List.of_seq) |>
  transpose |>
  List.map (fun l -> l |> List.to_seq |> String.of_seq)

let to_blocks block_separator l = let (blocks_start_rev, last_block) = List.fold_left 
  (fun (blocks_rev, block) next -> if block_separator next then ((List.rev block) :: blocks_rev, []) else (blocks_rev, (next :: block)))
  ([], [])
  l 
in 
  List.rev ((List.rev last_block) :: blocks_start_rev) 

let parse_operand_part part = parse_items_to_list parse_operand part |> Option.map List.rev

let parse_operands_new operand_lines = let operand_texts = operand_lines |> transposed_lines |> List.map String.trim in
  let operand_parts = operand_texts |> to_blocks (fun s -> String.length s = 0) in
  parse_items_to_list parse_operand_part operand_parts 


let solve_part_2 input = let maybe_computations = to_computations parse_operands_new input in
  match maybe_computations with
  None -> failwith "Invalid input!"
  | Some computations -> computations |> List.rev |> List.fold_left (fun acc next -> acc + execute_computation next) 0 |> string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"


let%test "parses example 1" = let expectation = [
  {operator = Mult; operands = [123;45;6]};
  {operator = Plus; operands = [328;64;98]};
  {operator = Mult; operands = [51;387;215]};
  {operator = Plus; operands = [64;23;314]};
] in
  let actual = Option.map (to_computations parse_operands) (parse example_input) in
  actual = Some (Some expectation)

let%test "parses example 2" = let expectation = [
  {operator = Plus; operands = [4;431;623]};
  {operator = Mult; operands = [175;581;32]};
  {operator = Plus; operands = [8;248;369]};
  {operator = Mult; operands = [356;24;1]};
] in
  let actual = Option.map (to_computations parse_operands_new) (parse example_input) in
  actual = Some (Some (List.rev expectation))


let%test "part 1 works for example" = let expectation = "4277556" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 6 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "4364617236318" in
  let input_text = InputReader.read_day_input 6 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 6 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "3263827" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 6 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "9077004354241" in
  let input_text = InputReader.read_day_input 6 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 6 on real inputs is " ^ actual);
  actual = expectation
