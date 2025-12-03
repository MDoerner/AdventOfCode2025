type id = int
type id_range = {start : id; stop : id}
type input_t = id_range list


let parse_range s = let re = Re.Perl.re "^(\\d+)-(\\d+)$" |> Re.compile in
  let maybe_groups = Re.exec_opt re s in
  Option.map (fun groups -> {start = (Re.Group.get groups 1) |> int_of_string; stop = (Re.Group.get groups 2) |> int_of_string }) maybe_groups
 
let parse s = let lines = String.split_on_char '\n' s in
  let first_line = List.hd lines in
  let range_specs = String.split_on_char ',' first_line in

  let maybe_ranges = List.map parse_range range_specs in
  List.fold_right (fun maybe_range acc -> match maybe_range, acc with
    None, _ -> None
    | _, None -> None
    | Some range, Some ranges -> Some (range :: ranges))
    maybe_ranges
    (Some [])



let rec power_of_ten n = if n = 0 then 1 else 10 * power_of_ten (n - 1)


type partition_range_spec = {start_prefix : int; stop_prefix : int; start_restrictions : int list; stop_restrictions : int list}

let string_parts n_partitions s = let len = String.length(s) in
  if not (len mod n_partitions = 0) then None else
    let part_length = len / n_partitions in
    let parts = Seq.ints 0 |> 
      Seq.take @@ n_partitions |> 
      Seq.map @@ (fun n -> String.sub s (n * part_length) part_length) in
    Some parts

let partitioning_range n_partitions {start; stop} = let start_string = string_of_int start in
  let start_length = String.length(start_string) in
  let start_prefix_length = (start_length + n_partitions - 1) / n_partitions in
  let start_prefix = if start_length mod n_partitions = 0 then
      String.sub start_string 0 start_prefix_length |> int_of_string
    else
      power_of_ten (start_prefix_length - 1)
    in
  let start_restrictions = if start_length mod n_partitions = 0 then
    start_string |> 
      string_parts @@ n_partitions |> 
      Option.get |> 
      Seq.map int_of_string |>
      List.of_seq
  else
    []
 in 
  let stop_string = string_of_int stop in
  let stop_length = String.length(stop_string) in
  let stop_prefix_length = stop_length / n_partitions in
  let stop_prefix = if stop_length mod n_partitions = 0 then
      String.sub stop_string 0 stop_prefix_length |> int_of_string
    else
      (power_of_ten stop_prefix_length) - 1
    in
  let stop_restrictions = if stop_length mod n_partitions = 0 then 
    stop_string |> 
      string_parts @@ n_partitions |> 
      Option.get |> 
      Seq.map int_of_string |>
      List.of_seq
  else
    []
  in
  {start_prefix; stop_prefix; start_restrictions; stop_restrictions}


let partition_base len n_partitions = if n_partitions <= 0 then 0 else
  if not (len mod n_partitions = 0) then 0 else
  power_of_ten (len / n_partitions)

let rec partition_factor base n_partitions = 
  if n_partitions <= 0 then 0 else 1 + base * (partition_factor base (n_partitions - 1))

let sum_between_same_length_prefixes lowest highest n_partitions = if highest < lowest then 0 else 
  let prefix_length = lowest |> string_of_int |> String.length in 
  let base = partition_base (n_partitions * prefix_length) n_partitions in
  let prefix_sum = highest * (highest + 1) / 2 - lowest * (lowest - 1) / 2 in
  prefix_sum * (partition_factor base n_partitions)


let is_repetition_with_fixed_count id n_partition = let id_string = id |> string_of_int in
  let len = id_string |> String.length in
    if not (len mod n_partition = 0) then false else
      let parts = id_string |> string_parts @@ n_partition |> Option.get in
      let (first_part, rest) = Seq.uncons parts |> Option.get in
      Seq.fold_left (fun same part -> same && String.equal part first_part) true rest

let is_repetition_with_lower_count id n_partition = if n_partition <= 2 then false else
    let is_lower_fixed = Seq.ints 2 |> Seq.take @@ (n_partition - 2) |> Seq.map (is_repetition_with_fixed_count @@ id) in
    Seq.fold_left (fun acc x -> acc || x) false is_lower_fixed

let sum_between_same_length_prefixes_wo_lower lowest highest n_partitions = if highest < lowest then 0 else 
   if n_partitions = 2 then sum_between_same_length_prefixes lowest highest n_partitions else  
    let candidate_prefixes = Seq.ints lowest |> Seq.take (highest - lowest + 1) in
    let prefix_len = lowest |> string_of_int |> String.length in
    let len = n_partitions * prefix_len in
    let candidates = Seq.map (fun prefix -> prefix |> string_of_int |> String.to_seq |> Seq.cycle |> Seq.take @@ len |> String.of_seq |> int_of_string) candidate_prefixes in
    Seq.fold_left (fun acc candidate -> if not (is_repetition_with_lower_count candidate n_partitions) then 
      acc + candidate else acc) 0 candidates



let rec sum_between_prefixes lowest highest n_partitions = if highest < lowest then 0 else
  let lowest_length = lowest |> string_of_int |> String.length in
  let highest_length = highest |> string_of_int |> String.length in
  if lowest_length = highest_length then sum_between_same_length_prefixes_wo_lower lowest highest n_partitions else
    let highest_of_length = (power_of_ten lowest_length) - 1 in
    let lowest_of_next_length = power_of_ten lowest_length in
    (sum_between_same_length_prefixes_wo_lower lowest highest_of_length n_partitions) + (sum_between_prefixes lowest_of_next_length highest n_partitions)

let sum_of_fixed_count_repetitions_in_range n_partitions {start_prefix; stop_prefix; start_restrictions; stop_restrictions} = let lowest = if 
  List.fold_left (fun indicator restriction -> if not (indicator = 0) then indicator else start_prefix - restriction) 0 start_restrictions < 0 then
    start_prefix + 1 else 
    start_prefix in
  let highest = if 
    List.fold_left (fun indicator restriction -> if not (indicator = 0) then indicator else stop_prefix - restriction) 0 stop_restrictions > 0 then 
      stop_prefix - 1 else 
      stop_prefix in
    sum_between_prefixes lowest highest n_partitions

let sum_of_fixed_count_repetitions_in_id_range id_range n_partitions = id_range |>  partitioning_range @@ n_partitions |> sum_of_fixed_count_repetitions_in_range @@ n_partitions

let solve_part_1 ranges = List.fold_left (fun acc range -> acc + sum_of_fixed_count_repetitions_in_id_range range 2) 0 ranges |> string_of_int


let sum_of_repetitions_in_id_range id_range = let {start; stop} = id_range in
    if start > stop then 0 else
      let stop_length = stop |> string_of_int |> String.length in
      let fixed_count_sums = Seq.ints 2 |>
        Seq.take @@ stop_length |>
        Seq.map (sum_of_fixed_count_repetitions_in_id_range @@ id_range)
    in
    Seq.fold_left (fun acc x -> acc + x) 0 fixed_count_sums





let solve_part_2 ranges = List.fold_left (fun acc range -> acc + sum_of_repetitions_in_id_range range) 0 ranges |> string_of_int

let solve_part p input = match p with
  DaySolver.Part1 -> solve_part_1 input
  | DaySolver.Part2 -> solve_part_2 input


let example_input = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
"




let%test "parses example" = let expectation = [
  {start = 11; stop = 22};
  {start = 95; stop = 115};
  {start = 998; stop = 1012};
  {start = 1188511880; stop = 1188511890};
  {start = 222220; stop = 222224};
  {start = 1698522; stop = 1698528};
  {start = 446443; stop = 446449};
  {start = 38593856; stop = 38593862};
  {start = 565653; stop = 565659};
  {start = 824824821; stop = 824824827};
  {start = 2121212118; stop = 2121212124}
  ] in
  let actual = parse example_input  in
  Option.equal (List.equal @@ (fun a b -> a = b)) actual (Some expectation)


let%test "part 1 works for example" = let expectation = "1227775554" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 2 on example is " ^ actual);
  actual = expectation


let%test "part 1 works for real input" = let expectation = "18700015741" in
  let input_text = InputReader.read_day_input 2 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_1 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 1 of day 2 on real inputs is " ^ actual);
  actual = expectation




let%test "part 2 works for example" = let expectation = "4174379265" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 2 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "20077272987" in
  let input_text = InputReader.read_day_input 2 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 2 on real inputs is " ^ actual);
  actual = expectation
