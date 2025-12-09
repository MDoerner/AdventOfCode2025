type coordinate = int

module Point = struct
  type t = coordinate * coordinate * coordinate
  let compare (x1, y1, z1) (x2, y2, z2) = match compare x1 x2 with
    0 -> (match compare y1 y2 with
          0 -> compare z1 z2
          | cy -> cy)
  | cx -> cx

  let square_distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)
end

type input_t = Point.t list







let parse_point s = let re = Re.Perl.re "^(\\d+),(\\d+),(\\d+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> 
    let x = (Re.Group.get groups 1) |> int_of_string in
    let y = (Re.Group.get groups 2) |> int_of_string in
    let z = (Re.Group.get groups 3) |> int_of_string in
    (x, y, z)) 
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


module type EDGE = sig
   type t = Point.t * Point.t
   val square_weight : t -> int
end

module Edge : EDGE = struct
  type t = Point.t * Point.t

  let square_weight (p1, p2) = Point.square_distance p1 p2
end




module DSU = struct
  module type OrderedType = Map.OrderedType

  module type S = sig
    type elt
    type t
    val empty : t
    val find : elt -> t -> (elt option * t)
    val union : elt -> elt -> t -> t      
    val roots_with_size : t -> (elt * int) list
  end

  module Make (Elt: OrderedType) : S with type elt = Elt.t  = struct
    module EltMap = Map.Make(Elt)
    type elt = Elt.t
    type cluster_size = int
    type t = (elt EltMap.t) * (cluster_size EltMap.t)

    let empty = (EltMap.empty, EltMap.empty)

    let rec root_and_path parents e = let parent = EltMap.find e parents in
      if parent = e then (e, []) else 
        let (root, path) = root_and_path parents parent in
        (root, parent :: path)

    let find e (parents, sizes) = if not (EltMap.mem e parents) then (None, (parents, sizes)) else
      let (root, path) = root_and_path parents e in
      let new_parents = List.fold_left 
        (fun current_parents p -> EltMap.update p (fun _ -> Some root) current_parents)
        parents
        path 
      in
      (Some root, (new_parents, sizes))

    let union a b store = let (maybe_a_root, store_a) = find a store in
      let (maybe_b_root, store_b) = find b store_a in
      let (parents, sizes) = store_b in
      match maybe_a_root with
      None -> 
        (match maybe_b_root with 
        None -> (parents |> EltMap.add b b |> EltMap.add a b, EltMap.add b 2 sizes)
        | Some b_root -> let b_size = EltMap.find b_root sizes in (EltMap.add a b_root parents, EltMap.update b_root (fun _ -> Some (b_size + 1)) sizes))
      | Some a_root -> let a_size = EltMap.find a_root sizes in (match maybe_b_root with 
        None -> (EltMap.add b a_root parents, EltMap.update a_root (fun _ -> Some (a_size + 1)) sizes)
        | Some b_root -> 
          if a_root = b_root then store_b else
            let b_size = EltMap.find b_root sizes in
            if a_size <= b_size then
              (EltMap.update a_root (fun _ -> Some b_root) parents, sizes |> EltMap.update b_root (fun _ -> Some (b_size + a_size)) |> EltMap.remove a_root)
            else
              (EltMap.update b_root (fun _ -> Some a_root) parents, sizes |> EltMap.update a_root (fun _ -> Some (b_size + a_size)) |> EltMap.remove b_root))
    
    let roots_with_size (_parents, root_sizes) = root_sizes |> EltMap.to_list
  end
end 

module PointDSU = DSU.Make(Point)


let cartesian_product l1 l2 = List.fold_left 
  (fun outer_acc x -> List.fold_left (fun inner_acc y -> ((x, y) :: inner_acc)) outer_acc l2) 
  [] 
  l1

let possible_connections points = cartesian_product points points |>
  List.filter (fun (a, b) -> Point.compare a b < 0)

let circuit_sizes edges = List.fold_left 
  (fun dsu (a, b) -> dsu |> PointDSU.union a b)
  PointDSU.empty
  edges |>
  PointDSU.roots_with_size |>
  List.map (fun (_root, size) -> size)

let solve_part1_for_shortest_edges n_edges points = points |> 
  possible_connections |>
  List.sort (fun e1 e2 -> compare (Edge.square_weight e1) (Edge.square_weight e2)) |> 
  List.take n_edges |>
  (*fun l -> let _ = List.iter (fun ((a, b, c), (x, y, z)) -> print_endline ("(" ^ string_of_int a ^ ", " ^ string_of_int b ^ ", " ^ string_of_int c ^ ") - (" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ string_of_int z ^ ")")) l in l |> *) 
  circuit_sizes |>
  List.sort (fun x y -> -compare x y) |>
  (*fun a -> let _ = List.iter (fun x -> print_string (string_of_int x ^ ", ")) a in a |>*) 
  List.take 3 |>
  List.fold_left (fun acc x -> acc * x) 1 |>
  string_of_int

let solve_part_1 points = solve_part1_for_shortest_edges 1000 points


let mst edges = let sorted_edges = List.sort (fun e1 e2 -> compare (Edge.square_weight e1) (Edge.square_weight e2)) edges in
  let (_clusters, full_mst) = List.fold_left 
    (fun (clusters, mst_part) edge -> let (a, b) = edge in 
      let (maybe_a_cluster, a_clusters) = PointDSU.find a clusters in
      match maybe_a_cluster with 
      None -> (a_clusters |> PointDSU.union a b, (edge :: mst_part))
      | Some a_cluster -> let (maybe_b_cluster, b_clusters) = PointDSU.find b a_clusters in
        match maybe_b_cluster with 
        None -> (b_clusters |> PointDSU.union a b, (edge :: mst_part))
        | Some b_cluster -> if a_cluster = b_cluster then (b_clusters, mst_part) else 
          (b_clusters |> PointDSU.union a b, (edge :: mst_part))) 
        (PointDSU.empty, [])
        sorted_edges
        in full_mst

let solve_part_2 points = let last_edge = points |> 
  possible_connections |> 
  mst |>
  List.hd in
  let ((x1, _, _), (x2, _, _)) = last_edge in
  string_of_int (x1 * x2)

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"


let%test "parses example" = let expectation = [
  (162,817,812);
  (57,618,57);
  (906,360,560);
  (592,479,940);
  (352,342,300);
  (466,668,158);
  (542,29,236);
  (431,825,988);
  (739,650,466);
  (52,470,668);
  (216,146,977);
  (819,987,18);
  (117,168,530);
  (805,96,715);
  (346,949,466);
  (970,615,88);
  (941,993,340);
  (862,61,35);
  (984,92,344);
  (425,690,689);
 ] in
  let actual = parse example_input  in
  actual = (Some expectation)


let%test "part 1 works for example" = let expectation = "40" in
  let input = parse example_input |> Option.get in
  let actual = solve_part1_for_shortest_edges 10 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 8 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "75680" in
  let input_text = InputReader.read_day_input 8 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 8 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "25272" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 8 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "8995844880" in
  let input_text = InputReader.read_day_input 8 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 8 on real inputs is " ^ actual);
  actual = expectation
