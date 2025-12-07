type coordinate = int

module Point = struct
  type t = coordinate * coordinate
  let compare (x1, y1) (x2, y2) = match compare y1 y2 with
    0 -> compare x1 x2
  | c -> c
end

module PointSet = Set.Make(Point)

type manifold = {start_column : coordinate; splitters : PointSet.t}
type input_t = manifold


let parse s = let lines = String.split_on_char '\n' s in
  if List.length lines = 0 then None else
    let (_, maybe_start_column, splitters) = List.fold_left
      (fun (y, maybe_start_c, occupied_points) line -> 
        let (_, maybe_start, new_occupied) = Seq.fold_left
          (fun (x, current_maybe_start, current_occupied) tile -> match tile with
            'S' -> (x + 1, Some x, current_occupied)
            | '^' -> (x + 1, current_maybe_start, PointSet.add (x, y) current_occupied)
            | _ -> (x + 1, current_maybe_start, current_occupied))
          (0, maybe_start_c, occupied_points)
          (String.to_seq line)
        in
        (y + 1, maybe_start, new_occupied))
      (0, None, PointSet.empty)
      lines
    in 
    match maybe_start_column with
    None -> None
    | Some start_column -> Some {start_column; splitters}

module ColumnSet = Set.Make(Int)

let hit_splitters manifold = let {start_column; splitters} = manifold in
  let sorted_splitters = splitters |> PointSet.to_list in
  let (active_splitters ,_rays) = List.fold_left 
    (fun (known, rays) splitter ->
      let (x,_y) = splitter in
        if not (ColumnSet.mem x rays) then (known, rays) else (PointSet.add splitter known, ColumnSet.(rays |> remove x |> add (x-1) |> add (x+1))))
    (PointSet.empty, ColumnSet.empty |> ColumnSet.add start_column)
    sorted_splitters
  in
  active_splitters


let solve_part_1 manifold = manifold |> 
  hit_splitters |>
  PointSet.cardinal |>
  string_of_int 

  
module ColumnMap = Map.Make(Int)

let paths_to_exits manifold = let {start_column; splitters} = manifold in
  let sorted_splitters = splitters |> PointSet.to_list in
  List.fold_left 
    (fun rays splitter ->
      let (x,_y) = splitter in
      let current_path_count = ColumnMap.find_opt x rays in
      match current_path_count with
      None -> rays 
      | Some path_count -> let count_updater = fun maybe_n -> match maybe_n with
        None -> Some path_count
        | Some n -> Some (n + path_count) 
    in (*let _ = print_string ("splitter (" ^ string_of_int x ^ ","  ^ string_of_int _y ^ ")  path_counts "); rays |> ColumnMap.to_list |> List.iter (fun (c, p) -> print_string ("{" ^ string_of_int c ^ "->"  ^ string_of_int p ^ "}")); print_endline "" in*)
      ColumnMap.(rays |> remove x |> update (x-1) count_updater |> update (x+1) count_updater))
    ColumnMap.(empty |> add start_column 1)
    sorted_splitters
  
let solve_part_2 manifold = manifold |> 
  paths_to_exits |>
  ColumnMap.to_list |>
  List.fold_left (fun acc (_column, path_count) -> acc + path_count) 0 |>
  string_of_int 

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"



let%test "part 1 works for example" = let expectation = "21" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 7 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "1543" in
  let input_text = InputReader.read_day_input 7 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 7 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "40" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 7 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "3223365367809" in
  let input_text = InputReader.read_day_input 7 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 7 on real inputs is " ^ actual);
  actual = expectation
