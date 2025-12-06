type id = int
type id_range = id * id
type input_t = (id_range list) * (id list)

let parse_id s = let re = Re.Perl.re "^\\d+$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> (Re.Group.get groups 0) |> int_of_string) 
    maybe_groups

  let parse_id_range s = let re = Re.Perl.re "^(\\d+)-(\\d+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> 
    let start = (Re.Group.get groups 1) |> int_of_string in
    let stop = (Re.Group.get groups 2) |> int_of_string in
    (start, stop)) 
    maybe_groups
 
let parse_to_list parse_item part = let maybe_items = part |> 
  String.split_on_char @@ '\n' |> 
  List.filter @@ (fun s ->  not (String.length s = 0)) |>
  List.map @@ parse_item in
    List.fold_right (fun maybe_item acc -> match maybe_item, acc with
      None, _ -> None
      | _, None -> None
      | Some item, Some items -> Some (item :: items))
      maybe_items
      (Some [])

let parse s = let re = Re.Perl.re "\\r?\\n\\r?\\n" |> Re.compile in
  let parts = Re.split re s in
  if List.length parts < 2 then None else
    let maybe_ranges = parse_to_list parse_id_range (List.hd parts) in
    match maybe_ranges with
      None -> None
      | Some ranges -> 
        let maybe_ids = parse_to_list parse_id (List.nth parts 1) in
          match maybe_ids with
            None -> None
            | Some ids -> Some (ranges, ids) 

let sorted_merged_ranges ranges = let sorted_ranges = List.sort 
  (fun (s1, e1) (s2, e2) -> match compare s1 s2 with 
    0 ->  compare e2 e1
    | c -> c) 
  ranges 
  in
  match sorted_ranges with
    [] -> []
    | (first :: rest) -> 
      let (reverse_merged_ranges, last) = List.fold_left
        (fun (known, candidate) next -> let (ns, ne) = next in
          let (cs, ce) = candidate in
            if ne <= ce then (known, candidate) else
              if ns <= ce then (known, (cs, ne)) else
                (candidate :: known, next)
          )
        ([], first)
        rest
      in 
      List.rev (last :: reverse_merged_ranges)

let sorted_in_range_ids sorted_disjoint_ranges sorted_ids = let (in_range_rev, _) = List.fold_left
      (fun (known_in_range_rev, remaining_ranges) id -> 
        let still_remaining_ranges = List.drop_while (fun (_start, stop) -> stop < id) remaining_ranges in
        match still_remaining_ranges with
          [] -> (known_in_range_rev, [])
          | (lowest_range :: _rest) ->
            let (start, _stop) = lowest_range in
              if id < start then (known_in_range_rev, still_remaining_ranges) else
                ((id ::known_in_range_rev), remaining_ranges)
      )
      ([], sorted_disjoint_ranges)
      sorted_ids
   in
   List.rev in_range_rev 

let solve_part_1 input = let (ranges, ids) = input in
  let sorted_disjoint_ranges = sorted_merged_ranges ranges in
  let sorted_ids = List.sort compare ids in
  let in_range_ids = sorted_in_range_ids sorted_disjoint_ranges sorted_ids in
  in_range_ids |> 
    List.length |> 
    string_of_int

let solve_part_2 input = let (ranges, _ids) = input in
  let sorted_disjoint_ranges = sorted_merged_ranges ranges in
  List.fold_left 
   (fun acc (start, stop) -> acc + (stop - start + 1))
   0
   sorted_disjoint_ranges |>
   string_of_int


let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "3-5
10-14
16-20
12-18

1
5
8
11
17
32
"


let%test "parses example" = let expectation = (
    [(3, 5); (10, 14); (16, 20); (12, 18)],
    [1;5;8;11;17;32]
 ) in
  let actual = parse example_input  in
  actual = (Some expectation)


let%test "part 1 works for example" = let expectation = "3" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 5 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "773" in
  let input_text = InputReader.read_day_input 5 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 5 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "14" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 5 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "332067203034711" in
  let input_text = InputReader.read_day_input 5 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 5 on real inputs is " ^ actual);
  actual = expectation
