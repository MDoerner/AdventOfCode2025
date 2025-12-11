type coordinate = int

module Point = struct
  type t = coordinate * coordinate
  let compare (x1, y1) (x2, y2) = match compare x1 x2 with
    0 -> compare y1 y2 
  | cx -> cx
end

type input_t = Point.t list


let parse_point s = let re = Re.Perl.re "^(\\d+),(\\d+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> 
    let x = (Re.Group.get groups 1) |> int_of_string in
    let y = (Re.Group.get groups 2) |> int_of_string in
    (x, y)) 
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

let parse s = parse_to_list parse_point s

let cartesian_product l1 l2 = List.fold_left 
  (fun outer_acc x -> List.fold_left (fun inner_acc y -> ((x, y) :: inner_acc)) outer_acc l2) 
  [] 
  l1

let possible_corner_pairs corners = cartesian_product corners corners |>
  List.filter (fun (a, b) -> Point.compare a b < 0)

let area rectangle = let ((x1, y1), (x2, y2)) = rectangle in
    (if x1 < x2 then x2 - x1 + 1 else x1 - x2 + 1) * (if y1 < y2 then y2 - y1 + 1 else y1 - y2 + 1)

let solve_part_1 red_points = red_points |> 
  possible_corner_pairs |> 
  List.map area |> 
  List.sort (fun a b -> -compare a b) |> 
  List.hd |> 
  string_of_int


let edges points = match points with
    [] -> []
    | [_] -> []
    | (first_point :: rest) ->
      let (edges_wo_last, last_point) = List.fold_left 
        (fun (known_edges, previous_point) next_point -> ((next_point, previous_point) :: known_edges, next_point))
        ([], first_point)
        rest 
      in
      (last_point, first_point) :: edges_wo_last

let is_vertical edge = let ((sx, _sy), (ex, _ey)) = edge in sx = ex

let is_horizontal edge = let ((_sx, sy), (_ex, ey)) = edge in sy = ey

let is_left_of_inside_of rect p = let ((cx1, _cy1), (cx2, _cy2)) = rect in 
  let (px, _py) = p in
  px <= cx1 && px <= cx2  

let is_right_of_inside_of rect p = let ((cx1, _cy1), (cx2, _cy2)) = rect in 
  let (px, _py) = p in
  px >= cx1 && px >= cx2  

let is_above_inside_of rect p = let ((_cx1, cy1), (_cx2, cy2)) = rect in 
  let (_px, py) = p in
  py <= cy1 && py <= cy2 

let is_below_inside_of rect p = let ((_cx1, cy1), (_cx2, cy2)) = rect in 
  let (_px, py) = p in
  py >= cy1 && py >= cy2 

let is_inside_vertical_bounds_of rect p = 
  not (is_below_inside_of rect p) && 
  not (is_above_inside_of rect p)

let is_inside_horizontal_bounds_of rect p = 
  not (is_left_of_inside_of rect p) && 
  not (is_right_of_inside_of rect p)

let is_inside_of rect p = 
  is_inside_vertical_bounds_of rect p && 
  is_inside_horizontal_bounds_of rect p 

let crosses_inside_of rect edge = let (s, e) = edge in
      is_inside_of rect s || 
      is_inside_of rect e ||
      is_horizontal edge && is_inside_vertical_bounds_of rect s && (is_left_of_inside_of rect s && is_right_of_inside_of rect e || is_left_of_inside_of rect e && is_right_of_inside_of rect s) ||
      is_vertical edge && is_inside_horizontal_bounds_of rect s && (is_above_inside_of rect s && is_below_inside_of rect e || is_above_inside_of rect e && is_below_inside_of rect s) 
      
let is_red_green edges rect = (*let violations = List.filter (crosses_inside_of rect) edges in 
  let _ = match violations with [] -> () | (first :: _rest) -> 
    let ((cx1, cy1), (cx2, cy2)) = rect in
    let ((sx, sy), (ex, ey)) = first in
    print_endline ("rect ((" ^ string_of_int cx1 ^ ", " ^ string_of_int cy1 ^ "), (" ^ string_of_int cx2 ^ ", " ^ string_of_int cy2 ^ "))   edge ((" ^ string_of_int sx ^ ", " ^ string_of_int sy ^ "), (" ^ string_of_int ex ^ ", " ^ string_of_int ey ^ "))")
  in*)
  not (List.exists (crosses_inside_of rect) edges)

  (* TODO: Fix this for the case of rectangles on the outside of the green area. *)
let solve_part_2 red_points = let green_edges = edges red_points in
  red_points |> 
  possible_corner_pairs |> 
  List.filter (is_red_green green_edges) |>
  List.map area |> 
  List.sort (fun a b -> -compare a b) |> 
  List.hd |> 
  string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"


let%test "parses example" = let expectation = [
  (7,1);
  (11,1);
  (11,7);
  (9,7);
  (9,5);
  (2,5);
  (2,3);
  (7,3)
 ] in
  let actual = parse example_input  in
  actual = (Some expectation)


let%test "part 1 works for example" = let expectation = "50" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 9 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "4786902990" in
  let input_text = InputReader.read_day_input 9 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 9 on real inputs is " ^ actual);
  actual = expectation



let%test "part 2 works for example" = let expectation = "24" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 9 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "1571016172" in
  let input_text = InputReader.read_day_input 9 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 9 on real inputs is " ^ actual);
  actual = expectation
