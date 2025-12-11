module Graph = struct
  module type OrderedType = Map.OrderedType

  module type S = sig
    type node
    type edge = node * node
    type t
    (* val empty : t *)
    (* val add_edge : edge -> t -> t *)
    (* val remove_edge : edge -> t -> t *)
    val remove_node : node -> t -> t
    val from_edges : edge list -> t
    val out_connections : node -> t -> node list
    val in_connections : node -> t -> node list
    val is_terminal : node -> t -> bool 
    (* val vertices : t -> node list *)
    val subgraph : node list -> t -> t
  end 

  module Make (Node: OrderedType) : S with type node = Node.t = struct
    module NodeMap = Map.Make(Node)
    module NodeSet = Set.Make(Node)
    type node = Node.t
    type edge = node * node
    type t = (NodeSet.t NodeMap.t) * (NodeSet.t NodeMap.t)

    let empty = (NodeMap.empty, NodeMap.empty)

    let reverse_edge (s, e) = (e, s)

    let add_edge_to_store (s, e) store = store |> 
      NodeMap.update s (fun maybe_connections -> match maybe_connections with
      | None -> NodeSet.empty |> NodeSet.add e |> Option.some
      | Some out_connections -> out_connections |> NodeSet.add e |> Option.some)

    let add_edge edge (out_connections, in_connections) = (
      add_edge_to_store edge out_connections,
      add_edge_to_store (reverse_edge edge) in_connections
    )

    let from_edges edges = List.fold_left 
      (fun graph edge -> add_edge edge graph)
      empty
      edges 

    let remove_edge_from_store (s, e) store =
      let maybe_connections = NodeMap.find_opt s store in
      match maybe_connections with
      | None -> store
      | Some connections -> 
          if not (NodeSet.mem e connections) then store else
            let new_connections = NodeSet.remove e connections in
            if NodeSet.is_empty new_connections then 
              NodeMap.remove s store
            else
              NodeMap.update s (fun _ -> Some new_connections) store
    
    let remove_edge edge (out_connections, in_connections) = (
      remove_edge_from_store edge out_connections,
      remove_edge_from_store (reverse_edge edge) in_connections
    ) 

    let out_connections node (out_connections, _in_connections) = let maybe_connections = NodeMap.find_opt node out_connections in
       match maybe_connections with
       | None -> []
       | Some connections -> 
          connections |>
          NodeSet.to_list

    let in_connections node (_out_connections, in_connections) = let maybe_connections = NodeMap.find_opt node in_connections in
       match maybe_connections with
       | None -> []
       | Some connections -> 
          connections |>
          NodeSet.to_list
    
    let remove_node node graph = let out_edges_to_remove = graph |> 
      out_connections node |> 
      List.map (fun e -> (node, e)) 
    in 
    let in_edges_to_remove = graph |> 
      in_connections node |> 
      List.map (fun e -> (e, node)) 
    in List.fold_left
          (fun current_graph edge -> remove_edge edge current_graph)
          graph
          (out_edges_to_remove @ in_edges_to_remove)

    let is_terminal node (out_connections, _in_connections) = not (NodeMap.mem node out_connections)

    let vertices (out_connections, in_connections) = 
      let out_nodes = out_connections |> NodeMap.bindings |> List.map (fun (k, _) -> k) |> NodeSet.of_list in
      let in_nodes = in_connections |> NodeMap.bindings |> List.map (fun (k, _) -> k) |> NodeSet.of_list in
      NodeSet.union out_nodes in_nodes |> NodeSet.to_list

    let subgraph nodes graph = let distinct_nodes = NodeSet.of_list nodes in
      let nodes_to_delete = vertices graph |> List.filter (fun node -> not (NodeSet.mem node distinct_nodes)) in
      List.fold_left 
        (fun current_graph node -> remove_node node current_graph)
        graph
        nodes_to_delete
  end
end

module Network = Graph.Make(String)

type input_t = Network.edge list


let parse_items_to_list parse_item item_texts = let maybe_items = item_texts |> 
  List.map @@ parse_item in
    List.fold_right (fun maybe_item acc -> match maybe_item, acc with
      None, _ -> None
      | _, None -> None
      | Some item, Some items -> Some (item :: items))
      maybe_items
      (Some [])

let parse_endpoints endpoints_text = endpoints_text |> 
  String.trim |> 
  String.split_on_char ' '

let parse_node_edges line = let re = Re.Perl.re "^([^:]+):\\s*(.+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re line in
  maybe_groups |> Option.map (fun groups -> 
    let node = (Re.Group.get groups 1) in
    let endpoints = (Re.Group.get groups 2) |> parse_endpoints in
    List.map (fun e -> (node, e)) endpoints)

let parse_to_list parse_item part = part |> 
  String.split_on_char @@ '\n' |> 
  List.filter @@ (fun s ->  not (String.length s = 0)) |>
  parse_items_to_list @@ parse_item

let parse s = s |> parse_to_list parse_node_edges |> Option.map List.flatten 


module NodeSet = Set.Make(String)
module NodeMap = Map.Make(String)

let rec reachable_nodes_impl network known_reachable next_nodes = 
  if NodeSet.is_empty next_nodes then known_reachable else 
    let next_successors = next_nodes |> 
      NodeSet.to_seq |> 
      Seq.fold_left
        (fun successors next -> successors |> NodeSet.add_seq (Network.out_connections next network |> List.to_seq))
        NodeSet.empty
    in
      let new_next_nodes = NodeSet.diff next_successors known_reachable in
      let new_known_reachable = NodeSet.union next_successors known_reachable in
      reachable_nodes_impl network new_known_reachable new_next_nodes

let reachable_nodes node network = let initially_known_reachable = (NodeSet.empty |> NodeSet.add node) in 
  reachable_nodes_impl network initially_known_reachable initially_known_reachable

let rec preset_nodes_impl network known_reachable next_nodes = 
  if NodeSet.is_empty next_nodes then known_reachable else 
    let next_successors = next_nodes |> 
      NodeSet.to_seq |> 
      Seq.fold_left
        (fun successors next -> successors |> NodeSet.add_seq (Network.in_connections next network |> List.to_seq))
        NodeSet.empty
    in
      let new_next_nodes = NodeSet.diff next_successors known_reachable in
      let new_known_reachable = NodeSet.union next_successors known_reachable in
      preset_nodes_impl network new_known_reachable new_next_nodes

let preset_nodes node network = let initially_known_reachable = (NodeSet.empty |> NodeSet.add node) in 
  preset_nodes_impl network initially_known_reachable initially_known_reachable

let nodes_on_paths_between start stop network = NodeSet.inter (reachable_nodes start network) (preset_nodes stop network)

let rec count_of_paths_wo_loops_between_impl network path_counts next_nodes = 
  if NodeSet.is_empty next_nodes then path_counts else
    let new_path_counts = next_nodes |> NodeSet.to_seq |> Seq.fold_left
      (fun counts node -> let n_paths = NodeMap.find node path_counts in
        let priors = Network.in_connections node network in
        List. fold_left 
          (fun inner_counts prior -> inner_counts |> 
            NodeMap.update prior (
              fun maybe_count -> match maybe_count with 
                | None -> Some n_paths
                | Some n -> Some (n + n_paths)))
          counts
          priors
          )
          path_counts 
    in 
    let new_network = next_nodes |> NodeSet.to_seq |> Seq.fold_left
      (fun net node -> Network.remove_node node net)
      network 
    in
    let new_next_nodes = next_nodes |> NodeSet.to_seq |> Seq.fold_left
      (fun priors node -> let node_priors = Network.in_connections node network |> List.to_seq in
        NodeSet.add_seq node_priors priors)
      NodeSet.empty |>
      NodeSet.filter (fun node -> Network.is_terminal node new_network) 
    in
    count_of_paths_wo_loops_between_impl new_network new_path_counts new_next_nodes



let count_of_paths_wo_loops_between start stop network = let relevant_nodes = nodes_on_paths_between start stop network in
    let subnet = Network.subgraph (NodeSet.to_list relevant_nodes) network in
    let path_counts = count_of_paths_wo_loops_between_impl subnet (NodeMap.of_list [(stop, 1)]) (NodeSet.of_list [stop]) in
    match NodeMap.find_opt start path_counts with
    | None -> 0
    | Some n -> n


let solve_part_1 edges = edges |> 
  Network.from_edges |>
  count_of_paths_wo_loops_between "you" "out" |>
  string_of_int


let count_of_paths_wo_loops_between_passing_through start stop (pause1 , pause2) network =
  let n_start_pause1 = count_of_paths_wo_loops_between start pause1 network in
  let n_start_pause2 = count_of_paths_wo_loops_between start pause2 network in
  let n_pause1_pause2 = count_of_paths_wo_loops_between pause1 pause2 network in
  let n_pause2_pause1 = count_of_paths_wo_loops_between pause2 pause1 network in
  let n_pause1_stop = count_of_paths_wo_loops_between pause1 stop network in
  let n_pause2_stop = count_of_paths_wo_loops_between pause2 stop network in
  (n_start_pause1 * n_pause1_pause2 * n_pause2_stop) + (n_start_pause2 * n_pause2_pause1 * n_pause1_stop)

let solve_part_2 edges = edges |> 
  Network.from_edges |>
  count_of_paths_wo_loops_between_passing_through "svr" "out" ("dac", "fft") |>
  string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
"

let example_input_2 = "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
"

(*
let edges_string edges = "[" ^ (edges |> List.map (fun (s, e) -> "(" ^ s ^ ", " ^ e ^ ")") |> String.concat "; ") ^ "]"
*)

let%test "parses example" = let expectation = [
  ("aaa", "you"); ("aaa", "hhh");
  ("you", "bbb"); ("you", "ccc");
  ("bbb", "ddd"); ("bbb", "eee");
  ("ccc", "ddd"); ("ccc", "eee"); ("ccc", "fff");
  ("ddd", "ggg"); 
  ("eee", "out"); 
  ("fff", "out"); 
  ("ggg", "out"); 
  ("hhh", "ccc"); ("hhh", "fff"); ("hhh", "iii");
  ("iii", "out")  
 ] in
  let actual = parse example_input in
  (*let _ = actual |> Option.get |> edges_string |> print_endline in
  let _ = expectation |> edges_string |> print_endline in*)
  actual = (Some expectation)


let%test "part 1 works for example" = let expectation = "5" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 11 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "724" in
  let input_text = InputReader.read_day_input 11 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 11 on real inputs is " ^ actual);
  actual = expectation



let%test "part 2 works for example" = let expectation = "2" in
  let input = parse example_input_2 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 11 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "473930047491888" in
  let input_text = InputReader.read_day_input 11 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 11 on real inputs is " ^ actual);
  actual = expectation
