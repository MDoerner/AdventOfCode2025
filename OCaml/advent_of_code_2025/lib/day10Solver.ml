
type diagram = int list
type wiring = diagram
type joltage = int
type machine = {indicators : diagram; buttons : wiring list; joltage_requirement : joltage list}


type input_t = machine list


let parse_items_to_list parse_item item_texts = let maybe_items = item_texts |> 
  List.map @@ parse_item in
    List.fold_right (fun maybe_item acc -> match maybe_item, acc with
      None, _ -> None
      | _, None -> None
      | Some item, Some items -> Some (item :: items))
      maybe_items
      (Some [])

let parse_indicator indicator_char = match indicator_char with 
    | '.' -> Some 0
    | '#' -> Some 1
    | _ -> None

let parse_diagram diagram_text = diagram_text |> 
  String.to_seq |> 
  List.of_seq |> 
  parse_items_to_list parse_indicator


let parse_wiring indicator_length wiring_text = if not (String.starts_with ~prefix:"(" wiring_text) || not (String.ends_with ~suffix:")" wiring_text) then None else
  wiring_text |> 
  fun s -> String.sub s 1 (String.length wiring_text - 2) |>
  String.split_on_char ',' |>
  parse_items_to_list int_of_string_opt |>
  fun a -> Option.bind a (fun indices -> if List.exists (fun index -> index < 0 || index >= indicator_length) indices then None else
    Seq.ints 0 |> 
    Seq.take indicator_length |> 
    Seq.map (fun n -> if List.mem n indices then 1 else 0) |>
    List.of_seq |> 
    Option.some)


let parse_buttons indicator_length buttons_text = buttons_text |>
    String.trim |>
    String.split_on_char ' ' |>
    parse_items_to_list (parse_wiring indicator_length)

let parse_joltage_requirement joltages_text = joltages_text |>
    String.split_on_char ',' |>
    parse_items_to_list int_of_string_opt

let parse_machine line = let re = Re.Perl.re "^\\[(.+)\\]\\s*([^{}]+)\\s*\\{([^{}]+)\\}$" |> Re.compile in
  let maybe_groups = Re.exec_opt re line in
  Option.bind maybe_groups (fun groups -> 
    let diagram_text = (Re.Group.get groups 1) in
    let maybe_indicators = parse_diagram diagram_text in
    Option.bind maybe_indicators (fun indicators ->
      let indicator_length = List.length indicators in

      let buttons_text = (Re.Group.get groups 2) in
      let maybe_buttons = parse_buttons indicator_length buttons_text in
      Option.bind maybe_buttons (fun buttons ->
        let joltages_text = (Re.Group.get groups 3) in
        let maybe_joltage_requirement = parse_joltage_requirement joltages_text in
        Option.bind maybe_joltage_requirement (fun joltage_requirement -> 
          if List.length joltage_requirement <> indicator_length then None else
            Some {indicators; buttons; joltage_requirement}))))

let parse_to_list parse_item part = part |> 
  String.split_on_char @@ '\n' |> 
  List.filter @@ (fun s ->  not (String.length s = 0)) |>
  parse_items_to_list @@ parse_item

let parse s = parse_to_list parse_machine s


let apply_wiring diagram wiring = List.map2 (fun a b -> (a + b) mod 2) diagram wiring

let rec least_presses_required_to_configure_impl target buttons times_pressed max_times =
  if times_pressed >= max_times then None else
    match buttons with
    | [] -> None
    | (first :: rest) -> 
      if first = target then Some (times_pressed + 1) else
        let with_first_applied = apply_wiring target first in
        let maybe_best_with_first = least_presses_required_to_configure_impl with_first_applied rest (times_pressed + 1) max_times in
        match maybe_best_with_first with 
        | None -> least_presses_required_to_configure_impl target rest times_pressed max_times
        | Some best_for_first -> 
            let maybe_best_for_rest = least_presses_required_to_configure_impl target rest times_pressed best_for_first in
            match maybe_best_for_rest with
            | None -> Some best_for_first
            | Some best_for_rest -> Some (min best_for_first best_for_rest)


let least_presses_required_to_configure machine = let {indicators = target; buttons; joltage_requirement = _} = machine in
  least_presses_required_to_configure_impl target buttons 0 (List.length buttons)


let solve_part_1 machines = machines |>
          List.map least_presses_required_to_configure |>
          List.map Option.get |>
          List.fold_left (fun acc x -> acc + x) 0 |>
          string_of_int

let reduce_joltage_requirement requirement wiring = List.map2 (fun a b -> a - b) requirement wiring

(*
let rec least_presses_required_to_charge_impl target buttons times_pressed max_times =
  if times_pressed >= max_times then None else
    if List.exists (fun button -> button = target) buttons then Some (times_pressed + 1) else
      List.fold_left (fun maybe_best_known button ->
        let remaining_target = reduce_joltage_requirement target button in
        if List.exists (fun target_joltage -> target_joltage < 0) remaining_target then maybe_best_known else 
          let maybe_best_for_remaining = match maybe_best_known with 
          | None -> least_presses_required_to_charge_impl remaining_target buttons (times_pressed + 1) max_times
          | Some best_known -> least_presses_required_to_charge_impl remaining_target buttons (times_pressed + 1) best_known in
          match maybe_best_for_remaining with
          | None -> maybe_best_known
          | Some best_for_remaining -> Some best_for_remaining
       )
       None
       buttons
*)
(*
let print_wire wire = wire |> List.map string_of_int |> String.concat ";" |> fun s -> "{" ^ s ^ "}" |> print_endline
*)
let rec least_presses_required_to_charge_impl target buttons times_pressed max_times =
  if times_pressed >= max_times then None else
    match buttons with
    | [] -> None
    | (first :: rest) -> 
      let maybe_best_without_first = least_presses_required_to_charge_impl target rest times_pressed max_times in
      let upper_limit = match maybe_best_without_first with | None -> max_times | Some best_without_first -> best_without_first in 
      let (_, maybe_best) = Seq.ints 1 |> 
      Seq.take (upper_limit - times_pressed) |> 
      Seq.fold_left 
        (fun (maybe_remaining_target, maybe_known_best) times_first_applied ->
          match maybe_remaining_target with
          | None -> (None, maybe_known_best)
          | Some remaining_target ->
            let new_remaining_target = reduce_joltage_requirement remaining_target first in
            if List.for_all (fun joltage -> joltage = 0) new_remaining_target then  (None, Some (times_pressed + times_first_applied)) else
              if List.exists (fun joltage -> joltage < 0) new_remaining_target then  (None, maybe_known_best) else
                let maybe_best_for_number_of_first = match maybe_known_best with
                | None -> least_presses_required_to_charge_impl new_remaining_target rest (times_pressed + times_first_applied) max_times
                | Some best_known -> least_presses_required_to_charge_impl new_remaining_target rest (times_pressed + times_first_applied) best_known in
                match maybe_best_for_number_of_first with 
                | None -> (Some new_remaining_target, maybe_known_best)
                | Some best_for_number_of_first -> (Some new_remaining_target, Some best_for_number_of_first))
        (Some target, maybe_best_without_first)
      in
      maybe_best
      

      

let total_joltage joltages = List.fold_left (fun acc x -> acc + x) 0 joltages

let least_presses_required_to_charge machine = let {indicators = _; buttons; joltage_requirement = target} = machine in
  let total_target_joltage = total_joltage target in 
  least_presses_required_to_charge_impl target buttons 0 total_target_joltage

  (* TODO: Write an equation solver to finally ba able to solve part 2! *)
let solve_part_2 machines = machines |>
          List.map least_presses_required_to_charge |>
          List.map Option.get |>
          List.fold_left (fun acc x -> acc + x) 0 |>
          string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"


let%test "parses example" = let expectation = [
  {indicators = [0;1;1;0]; buttons =[[0;0;0;1]; [0;1;0;1]; [0;0;1;0]; [0;0;1;1]; [1;0;1;0]; [1;1;0;0]]; joltage_requirement = [3;5;4;7]};
  {indicators = [0;0;0;1;0]; buttons =[[1;0;1;1;1]; [0;0;1;1;0]; [1;0;0;0;1]; [1;1;1;0;0]; [0;1;1;1;1]]; joltage_requirement = [7;5;12;7;2]};
  {indicators = [0;1;1;1;0;1]; buttons =[[1;1;1;1;1;0]; [1;0;0;1;1;0]; [1;1;1;0;1;1]; [0;1;1;0;0;0]]; joltage_requirement = [10;11;11;5;10;5]}
 ] in
  let actual = parse example_input  in
  actual = (Some expectation)


let%test "part 1 works for example" = let expectation = "7" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 10 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "422" in
  let input_text = InputReader.read_day_input 10 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 10 on real inputs is " ^ actual);
  actual = expectation



let%test "part 2 works for example" = let expectation = "33" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on example is " ^ actual);
  actual = expectation

(*
let%test "part 2 works for real input" = let expectation = "" in
  let input_text = InputReader.read_day_input 10 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on real inputs is " ^ actual);
  actual = expectation
*)