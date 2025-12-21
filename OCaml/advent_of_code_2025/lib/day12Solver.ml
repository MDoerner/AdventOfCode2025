type package_size = int
type tree = {dims : int * int; counts : int list}

type input_t = {package_types : package_size list; trees : tree list} 


let parse_items_to_list parse_item item_texts = let maybe_items = item_texts |> 
  List.map @@ parse_item in
    List.fold_right (fun maybe_item acc -> match maybe_item, acc with
      None, _ -> None
      | _, None -> None
      | Some item, Some items -> Some (item :: items))
      maybe_items
      (Some [])

let parse_package_counts counts_text = counts_text |> 
  String.trim |> 
  String.split_on_char ' ' |>
  parse_items_to_list int_of_string_opt

let parse_tree line = let re = Re.Perl.re "^(\\d+)x(\\d+):\\s*(.+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re line in
    Option.bind maybe_groups (fun groups -> 
    let width = (Re.Group.get groups 1) |> int_of_string in
    let height = (Re.Group.get groups 2) |> int_of_string in
    let maybe_package_counts = (Re.Group.get groups 3) |> parse_package_counts in
    maybe_package_counts |>
      Option.map (fun package_counts -> {dims = (width, height); counts = package_counts}))

let parse_to_list parse_item part = part |> 
  String.split_on_char @@ '\n' |> 
  List.filter @@ (fun s ->  not (String.length s = 0)) |>
  parse_items_to_list @@ parse_item

let parse_trees s = s |> parse_to_list parse_tree

let parse_package package_definition = package_definition |> 
  String.to_seq |>
  Seq.filter (fun c -> c = '#') |>
  Seq.length |>
  Option.some

let parse_packages package_definitions = package_definitions |> parse_items_to_list parse_package

let parse s = let re = Re.Perl.re "\\r?\\n\\r?\\n" |> Re.compile in
  let parts = Re.split re s in
  if List.length parts <> 7 then None else
    let rev_parts = parts |> List.rev in
    match rev_parts with
    | [] -> None
    | (trees_text :: rev_package_definitions) ->
      let maybe_trees = parse_trees trees_text in
      match maybe_trees with
      | None -> None
      | Some trees ->
        let maybe_package_types = rev_package_definitions |> List.rev |> parse_packages in
        match maybe_package_types with 
        | None -> None
        | Some package_types -> 
          Some {package_types; trees}


let is_trivially_packable tree = let {dims = (width, height); counts} = tree in
          (* Packages always fit into 3x3 squares. *)
          let trivial_horizontal_package_count = width / 3 in
          let trivial_vertical_package_count = height / 3 in
          let total_package_count = List.fold_left (fun acc x -> acc + x) 0 counts in
            total_package_count <= trivial_horizontal_package_count * trivial_vertical_package_count

let is_properly_overpacked packages tree = let {dims = (width, height); counts} = tree in
  let total_available_space = width * height in
  let total_required_space = List.fold_left2 (fun acc size n -> acc + n * size) 0 packages counts in
  if total_available_space < total_required_space then true else
    let _ = string_of_int width ^ "x" ^ string_of_int height ^ ": " ^ (counts |> List.map string_of_int |> String.concat " ")  ^ " | available = " ^ string_of_int total_available_space ^ " | required = " ^ string_of_int total_required_space  |> print_endline in 
    let trivial_package_count = (width / 3) * (height / 3) in
    let total_package_count = List.fold_left (fun acc x -> acc + x) 0 counts in
    let _ = "trivial = " ^ string_of_int trivial_package_count ^ " | packages = " ^ string_of_int total_package_count |> print_endline in
    false 

let print_debug_info packages tree = let {dims = (width, height); counts} = tree in
  let total_available_space = width * height in
  let total_required_space = List.fold_left2 (fun acc size n -> acc + n * size) 0 packages counts in
  let total_package_count = List.fold_left (fun acc x -> acc + x) 0 counts in
  let trivial_package_count = (width / 3) * (height / 3) in
  let _ = "packages = [" ^ (packages |> List.map string_of_int |> String.concat "; ") ^ "]" |> print_endline in
  let _ = "trees = " ^ string_of_int width ^ "x" ^ string_of_int height ^ ": " ^ (counts |> List.map string_of_int |> String.concat " ") |> print_endline in
  let _ = "available = " ^ string_of_int total_available_space ^ " | required = " ^ string_of_int total_required_space  |> print_endline in
  let _ = "trivial = " ^ string_of_int trivial_package_count ^ " | packages = " ^ string_of_int total_package_count |> print_endline in
  ()

let is_packable packages tree = if is_trivially_packable tree then true else
  if is_properly_overpacked packages tree then false else
    let _ = print_debug_info packages tree in
    failwith "Case requiring smart packing not implemented!"


let solve_part_1 input = let {package_types; trees} = input in
    trees |> 
      List.filter (is_packable package_types) |>
      List.length |>
      string_of_int

let solve_part_2 _ = failwith "The last day of AdventOfCode does not have a part 2!"

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
"

let%test "parses example" = let expectation = {package_types = [7;7;7;7;7;7]; trees = [
  {dims = (4,4); counts = [0; 0; 0; 0; 2; 0]};
  {dims = (12,5); counts = [1; 0; 1; 0; 2; 2]};
  {dims = (12,5); counts = [1; 0; 1; 0; 3; 2]}
]} in
  let actual = parse example_input in
  actual = (Some expectation)


let%test "part 1 works for real input" = let expectation = "724" in
  let input_text = InputReader.read_day_input 12 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 11 on real inputs is " ^ actual);
  actual = expectation
