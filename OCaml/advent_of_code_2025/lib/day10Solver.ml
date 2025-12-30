
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

(*
let reduce_joltage_requirement requirement wiring = List.map2 (fun a b -> a - b) requirement wiring

(* This is very naive!
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
*)*)
(*
let print_wire wire = wire |> List.map string_of_int |> String.concat ";" |> fun s -> "{" ^ s ^ "}" |> print_endline
*)
(* Too naive!!!! *)
(*
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
*)

module type Ring = sig
    type t
    val zero : t
    val one : t 
    val add : t -> t -> t
    val neg : t -> t
    val mul : t -> t -> t
    (* mathematically some rules on add and mult apply*)
  end

module Matrix = struct
  module type S = sig
    type elt
    type t
    val add : t -> t -> t 
    val mul : t -> t -> t
    val scalar_mul : elt -> t -> t
    val columns : t -> elt list list
    val of_columns : elt list list -> t
    val of_association : int * int -> ((int * int) * elt) list -> t
    val dimensions : t -> int * int
    val transpose : t -> t
  end

  module Make(Elt : Ring) : S with type elt = Elt.t = struct
    type elt = Elt.t
    type t = elt list list
    let add a b = List.map2 (fun x y -> List.map2 Elt.add x y) a b 
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
    let mul a b = let a_t = transpose a in
      List.fold_left (fun columns_rev b_j -> 
        let next_column = List.map (fun a_i -> List.fold_left2 (fun acc x y -> Elt.add acc (Elt.mul x y)) Elt.zero a_i b_j) a_t in
        (next_column :: columns_rev)) [] b |>
        List.rev
    let scalar_mul x a = a |> List.map (fun a_t -> a_t |> List.map (Elt.mul x)) 
    let dimensions a = let n = List.length a in
        if n = 0 then (0 , 0) else
          let m = a |> List.hd |> List.length in 
          (m, n)
    let columns a = a
    let of_columns a = a
    let of_association dims associations = let (m, n) = dims in
        Seq.ints 0 |>
        Seq.take n |>
        Seq.fold_left (fun columns_rev j ->
          let column = Seq.ints 0 |>
          Seq.take m |>
          Seq.fold_left (fun items_rev i ->
              let item = match List.assoc_opt (i,j) associations with
              | None -> Elt.zero
              | Some v -> v in
              (item :: items_rev))
            [] |>
            List.rev in
            (column :: columns_rev)) 
          [] |>
          List.rev
  end
end



module type PID (* Principal Ideal Domain *)= sig
  include Ring
  val gcd_with_bezout : t -> t -> t * t * t
  (* We only need the Bezout decomposition here, but in the intended application only being an actual PID will guarantee that the algorithm terminates *)
  val divides : t -> t -> bool
  val div : t -> t -> t option
end 


module type INT_PID = sig
  include PID
  val is_neg : t -> bool
  val to_int : t -> int
  val of_int : int -> t
  val remainder : t -> t -> t
end


module IntPID : INT_PID = struct
  type t = int
  let zero = Int.zero
  let one = Int.one
  let add a b = Int.add a b
  let neg a = Int.neg a
  let mul a b = Int.mul a b
  let divides a b = (a mod b) = 0
  let div a b = if divides a b then Some (Int.div a b) else None

  let remainder a b = ((a mod b) + b) mod b (* This guarantees that the remainder is always positive. *)

  let rec extended_euclidean_algorithm a b = 
    if a = 0 then 
      (b, 0, 1)
    else 
      let (gcd, x, y) = extended_euclidean_algorithm (remainder b a) a in
      (gcd, y - (b/a) * x, x)

  let gcd_with_bezout a b = let (gcd, sigma, tau) = extended_euclidean_algorithm a b in
      let _ = if gcd <> sigma * a + tau * b then  
        ( "a = " ^ string_of_int a |> print_endline;
         "b = " ^ string_of_int b |> print_endline;
         "gcd = " ^ string_of_int gcd |> print_endline;
         "sigma = " ^ string_of_int sigma |> print_endline;
         "tau = " ^ string_of_int tau |> print_endline;
        failwith "Incorrect extended euclidean algorithm!") in 
      (gcd, sigma, tau)

  let is_neg x = x < 0
  let to_int x = x
  let of_int x = x
end

module IntMatrix = Matrix.Make(IntPID)  

let print_matrix a = a |> IntMatrix.transpose |> IntMatrix.columns |> List.iter
  (fun row -> "[" ^ (row |> List.map IntPID.to_int |> List.map string_of_int |> String.concat " ") ^ "]" |> print_endline)


let rec find_next_column_index_impl cols t j_t = match cols with 
  | [] -> None
  | (next :: rest)  -> 
    if not (List.for_all (fun x -> x = IntPID.zero) (next |> List.drop t)) then Some j_t else
       find_next_column_index_impl rest t (j_t + 1)

let find_next_column_index a t j_t = let remaining_columns = a |> IntMatrix.columns |> List.drop j_t in
  find_next_column_index_impl remaining_columns t j_t


let exchange_rows a u t k = let (m, _n) = IntMatrix.dimensions a in
    let diagonal_entries = Seq.ints 0 |>
      Seq.take m |>
      Seq.fold_left (fun entries i -> (((i,i), IntPID.one):: entries)) [] in
    let adjustment_entries = diagonal_entries |>
      List.filter (fun ((i, _), _) -> i <> k && i <> t) |>
      List.cons ((t, k), IntPID.one) |>
      List.cons ((k, t), IntPID.one) in
    let adjustment_matrix = IntMatrix.of_association (m, m) adjustment_entries in
    let new_u = IntMatrix.mul adjustment_matrix u in
    let new_a = IntMatrix.mul adjustment_matrix a in
    (new_a, new_u)


let find_pivot a u t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  if not (List.nth col t = IntPID.zero) then (a, u) else
    let pivot_row_offset = List.find_index (fun x -> not (x = IntPID.zero)) (col |> List.drop t) |>
      Option.get in (* This function is only used on vectors with at least one non-zero entry at index t or above. *)
      (* let _ = "switching rows " ^ string_of_int t ^ " and " ^ string_of_int (pivot_row_offset + t) |> print_endline in *)
    exchange_rows a u t (pivot_row_offset + t)

let improve_pivot_once a u t k a_t a_k = let (gcd, sigma, tau) = IntPID.gcd_with_bezout a_t a_k in
    let alpha = IntPID.div a_t gcd |> Option.get in
    let gamma = IntPID.div a_k gcd |> Option.get in
    let (n, _m) = IntMatrix.dimensions a in
  let diagonal_entries = Seq.ints 0 |>
      Seq.take n |>
      Seq.fold_left (fun entries i -> (((i,i), IntPID.one):: entries)) [] in
    let adjustment_entries = diagonal_entries |>
      List.filter (fun ((i, _), _) -> i <> k && i <> t) |>
      List.cons ((t, t), sigma) |>
      List.cons ((k, k), alpha) |>
      List.cons ((k, t), IntPID.neg gamma) |>
      List.cons ((t, k), tau) in 
    let adjustment_matrix = IntMatrix.of_association (n, n) adjustment_entries in
    let new_u = IntMatrix.mul adjustment_matrix u in
    let new_a = IntMatrix.mul adjustment_matrix a in
    (new_a, new_u)

(* This is a special step necessary because we are not working over a field and thus, row indices are not always divisible by the pivot. 
   The Bezout identity, which always exists in a PID, is used to convert the matrix by left-multiplication with a unimodular matrix into one with a 
   pivot dividing all row indices below. This terminates because a PID is Noetherian and, thus, the tower of ideal generated by the successive pivots 
   must become stationary. *)
let rec improve_pivot a u t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  let relevant_col = col |> List.drop t in
  match relevant_col with
  | [] -> failwith "improve_pivot must only be called for a valid row index t!"
  | (pivot :: to_divide) ->
    let maybe_indivisible_offset = List.find_index (fun x -> not (IntPID.divides x pivot)) to_divide in
    match maybe_indivisible_offset with
    | None -> (a, u)
    | Some k ->
      let indivisible = List.nth to_divide k in
      let (improved_a, improved_u) = improve_pivot_once a u t (t + k + 1) pivot indivisible in
      improve_pivot improved_a improved_u t j_t

let eliminate_row a u t k factor = let (m, _n) = IntMatrix.dimensions a in
  let diagonal_entries = Seq.ints 0 |>
      Seq.take m |>
      Seq.fold_left (fun entries i -> (((i,i), IntPID.one):: entries)) [] in
    let adjustment_entries = diagonal_entries |>
      List.cons ((k, t), IntPID.neg factor) in 
    let adjustment_matrix = IntMatrix.of_association (m, m) adjustment_entries in
    let new_u = IntMatrix.mul adjustment_matrix u in
    let new_a = IntMatrix.mul adjustment_matrix a in
    (new_a, new_u)

let eliminate_rows a u t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  let relevant_col = col |> List.drop t in
  match relevant_col with
  | [] -> failwith "eliminate_rows must only be called for a valid row index t!"
  | (pivot :: to_eliminate) ->
  let (a_new, u_new, _) = List.fold_left (fun (a_curr, u_curr, k) x -> if x = IntPID.zero then (a_curr, u_curr, k + 1) else
    let factor = IntPID.div x pivot |> Option.get in (* This is ok because the function is only called in case the pivot divides all entires in the same column with higher index. *)
    let (a_next, u_next) = eliminate_row a_curr u_curr t k factor in
    (a_next, u_next, k + 1))
    (a, u, t+1)
    to_eliminate 
  in
  (a_new, u_new)

let column_phase a u t j_t = let (a_impr, u_impr) = improve_pivot a u t j_t in
  eliminate_rows a_impr u_impr t j_t

let row_phase a v t j_t = let a_tr = IntMatrix.transpose a in
  let v_tr = IntMatrix.transpose v in
  let (a_tr_new, v_tr_new) = column_phase a_tr v_tr j_t t in
  let a_new = IntMatrix.transpose a_tr_new in
  let v_new = IntMatrix.transpose v_tr_new in
  (a_new, v_new)

  



let sign_switch_matrix n t = let diagonal_entries = Seq.ints 0 |>
      Seq.take n |>
      Seq.fold_left (fun entries i -> (((i,i), IntPID.one):: entries)) [] in
    let adjustment_entries = diagonal_entries |>
      List.filter (fun ((i, _), _) -> i <> t) |>
      List.cons ((t, t), IntPID.neg IntPID.one) in
    IntMatrix.of_association (n, n) adjustment_entries

let switch_row_sign a u t = let (n, _m) = IntMatrix.dimensions a in
    let adjustment_matrix = sign_switch_matrix n t in
    let new_u = IntMatrix.mul adjustment_matrix u in
    let new_a = IntMatrix.mul adjustment_matrix a in
    (new_a, new_u) 

let switch_column_sign a v t = let (_n, m) = IntMatrix.dimensions a in
    let adjustment_matrix = sign_switch_matrix m t in
    let new_v = IntMatrix.mul v adjustment_matrix in
    let new_a = IntMatrix.mul a adjustment_matrix in
    (new_a, new_v) 

let adjust_row_signs_below a u t j_t = let relevant_col = a |> IntMatrix.columns |> (fun ls -> List.nth ls j_t) |> List.drop t in
      let (a_new, u_new, _ ) = List.fold_left
        (fun (a_curr, u_curr, k) x -> if not (IntPID.is_neg x) then (a_curr, u_curr, k + 1) else 
          let (a_next, u_next) = switch_row_sign a_curr u_curr k in 
            (a_next, u_next, k + 1))
        (a, u, t)
        relevant_col
    in
    (a_new, u_new)

let adjust_column_signs a v t = let row = a |> IntMatrix.transpose |> IntMatrix.columns |> (fun ls -> List.nth ls t) in
      let (a_new, v_new, _ ) = List.fold_left
        (fun (a_curr, v_curr, k) x -> if not (IntPID.is_neg x) then (a_curr, v_curr, k + 1) else 
          let (a_next, v_next) = switch_column_sign a_curr v_curr k in 
            (a_next, v_next, k + 1))
        (a, v, 0)
        row
    in
    (a_new, v_new)

let adjust_signs a u v t j_t = let (a_row, u_new) = adjust_row_signs_below a u t j_t in
    let (a_new, v_new) = adjust_column_signs a_row v t in
    (a_new, u_new, v_new) 

let rec index_phase a u v t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  if (col |> List.drop (t + 1) |> List.for_all (fun x -> x = IntPID.zero)) &&  
    (let row = List.nth (a |> IntMatrix.transpose |> IntMatrix.columns) t in
    row |> List.drop (j_t + 1) |> List.for_all (fun x -> x = IntPID.zero)) then (a, u, v) else
      let (a_col, u_new) = column_phase a u t j_t in
      let (a_col_sign, v_sign) = adjust_column_signs a_col v t in (* Technically, this is unnecessary, but allows to keep make some simplifcations over integers. *)
      let (a_row, v_new) = row_phase a_col_sign v_sign t j_t in 
      let (a_row_sign, u_new_sign) = adjust_row_signs_below a_row u_new t j_t in (* Technically, this is unnecessary, but allows to keep make some simplifcations over integers. *)
      index_phase a_row_sign u_new_sign v_new t j_t     

let rec digonalization_phase a u v t j_t_candidate = let maybe_next_column = find_next_column_index a t j_t_candidate in
      match maybe_next_column with 
      | None -> (a, u, v)
      | Some j_t -> 
        (* let _ = print_endline ("a " ^ string_of_int t); print_matrix a in *)
        let (a_pivot, u_pivot) = find_pivot a u t j_t in
        (* let _ = print_endline ("a_pivot " ^ string_of_int t); print_matrix a_pivot; print_endline "" in *)
        let (a_sign, u_sign, v_sign) = adjust_signs a_pivot u_pivot v t j_t in (* Technically, this is unnecessary, but allows to keep make some simplifcations over integers. *)
        (* let _ = print_endline ("a_sign " ^ string_of_int t); print_matrix a_sign; print_endline "" in *)
        let (a_new, u_new, v_new) = index_phase a_sign u_sign v_sign t j_t in
        digonalization_phase a_new u_new v_new (t + 1) (j_t + 1)

let reorder_columns_phase a v = let a_tr = IntMatrix.transpose a in
  let v_tr = IntMatrix.transpose v in
  let (a_tr_new, v_tr_new, _) = List.fold_left 
    (fun (a_curr, v_curr, t) a_t -> 
      let maybe_diagonal_index = List.find_index (fun x -> x <> IntPID.zero) a_t in
      match maybe_diagonal_index with
      | None -> (a_curr, v_curr, t + 1)
      | Some k -> if k = t then (a_curr, v_curr, t + 1) else
          let (a_next, v_next) = exchange_rows a_curr v_curr t k in
          (a_next, v_next, t + 1))
    (a_tr, v_tr, 0)
    (IntMatrix.columns a_tr) in
  let a_new = IntMatrix.transpose a_tr_new in
  let v_new = IntMatrix.transpose v_tr_new in
  (a_new, v_new)

let add_column a v t k = let (_m, n) = IntMatrix.dimensions a in
  let diagonal_entries = Seq.ints 0 |>
      Seq.take n |>
      Seq.fold_left (fun entries i -> (((i,i), IntPID.one):: entries)) [] in
    let adjustment_entries = diagonal_entries |>
      List.cons ((t, k), IntPID.one) in 
    let adjustment_matrix = IntMatrix.of_association (n, n) adjustment_entries in
    let new_v = IntMatrix.mul v adjustment_matrix in
    let new_a = IntMatrix.mul a adjustment_matrix in
    (new_a, new_v)

let rec divisibility_phase_impl a u v max_t t d_prior = if t >= max_t then (a, u, v) else
  let d = List.nth (List.nth (IntMatrix.columns a) t) t in (* This is kind of inefficient but good enough for this toy problem. *)
    if d = IntPID.zero then (a, u , v) else
      if IntPID.divides d d_prior then divisibility_phase_impl a u v max_t (t + 1) d else
        let (a_added, v_added) = add_column a v t (t-1) in
        let (a_new, u_new, v_new) = index_phase a_added u v_added (t-1) (t-1) in
          divisibility_phase_impl a_new u_new v_new max_t 0 IntPID.one

let divisibility_phase a u v = let (m, n) = IntMatrix.dimensions a in
  let max_t = min m n in
   divisibility_phase_impl a u v max_t 0 IntPID.one

let diagonal_entries a = let (m, n) = IntMatrix.dimensions a in
  let max_t = min m n in
  let (diags_rev, _) = a |> IntMatrix.columns |> List.fold_left 
    (fun (known_rev, t) col -> if t >= max_t then (known_rev, t + 1) else
      let diag = List.nth col t in
      ((diag :: known_rev), t + 1)) 
    ([], 0) in
    List.rev diags_rev

let diagonal_matrix n = let diagonal_entries = Seq.ints 0 |>
      Seq.take n |>
      Seq.fold_left (fun entries i -> (((i, i), IntPID.one):: entries)) [] in
      IntMatrix.of_association (n, n) diagonal_entries

(* Smith normal form of A of dimension (m x n) is a triple of 
   a diagonal matrix D of dimension (m x n) (filled with zero columns to the right),
   an invertible matrix U of dimension (m x m) and
   an invertible matrix V of dimension (n x n) such that 
   D = UAV *)
let smith_normal_form a = let (m, n) = IntMatrix.dimensions a in
  (* let _ = print_endline "a"; print_matrix a in *)
   let u = diagonal_matrix m in
   let v = diagonal_matrix n in
   (* let _ = "dims = (" ^ string_of_int m ^ " x " ^ string_of_int n ^ ")" |> print_endline in *)
   let (a_diag, u_diag, v_diag) = digonalization_phase a u v 0 0 in
   let validation_matrix = IntMatrix.add a_diag (v_diag |> IntMatrix.mul a |> IntMatrix.mul u_diag |> IntMatrix.scalar_mul (IntPID.neg IntPID.one)) in
   let _ = if validation_matrix |> IntMatrix.columns |> List.exists (fun col -> col |> List.exists (fun x -> x <> IntPID.zero)) then 
    (print_endline "Smith Normal Form inconsistent after diagonalization phase!"; print_endline "a"; print_matrix a; print_endline "a_diag"; print_matrix a_diag; print_endline "u"; print_matrix u_diag; print_endline "v"; print_matrix v_diag) in
   (* let _ = print_endline "a_diag"; print_matrix a_diag in *)
   let (a_order, v_order) = reorder_columns_phase a_diag v_diag in
   let validation_matrix = IntMatrix.add a_order (v_order |> IntMatrix.mul a |> IntMatrix.mul u_diag |> IntMatrix.scalar_mul (IntPID.neg IntPID.one)) in
   let _ = if validation_matrix |> IntMatrix.columns |> List.exists (fun col -> col |> List.exists (fun x -> x <> IntPID.zero)) then 
    (print_endline "Smith Normal Form inconsistent after order phase!"; print_endline "a"; print_matrix a; print_endline "a_order"; print_matrix a_order; print_endline "u"; print_matrix u_diag; print_endline "v"; print_matrix v_order) in
   (* let _ = print_endline "a_order"; print_matrix a_order in *)
   let (a_div, u_div, v_div) = divisibility_phase a_order u_diag v_order in
   let _ = if validation_matrix |> IntMatrix.columns |> List.exists (fun col -> col |> List.exists (fun x -> x <> IntPID.zero)) then 
    (print_endline "Smith Normal Form inconsistent after divisibility phase!"; print_endline "a"; print_matrix a; print_endline "a_div"; print_matrix a_div; print_endline "u"; print_matrix u_div; print_endline "v"; print_matrix v_div) in
   (* let _ = print_endline "a_div"; print_matrix a_div in *)
   (a_div, u_div, v_div)


let vector_print_string v = let values = List.map IntPID.to_int v in
  let total = List.fold_left (fun acc x -> acc + x) 0 values in
  "   [" ^ String.concat "; " (values |> List.map string_of_int) ^ "] with total = " ^ string_of_int total


type linear_solution = {base_solution : IntPID .t list; free_vectors : IntPID.t list list}

(* b = Ax <=> Ub = UAx = UAV(V^-1)x = D(V^-1)x = Dy with y = (V^-1)x *)
let solve_via_smith_normal_form a b = let (d, u, v) = smith_normal_form a in
  let b_transformed = [b] |> IntMatrix.of_columns |> IntMatrix.mul u |> IntMatrix.columns |> List.hd in
  (* let _ = "transformed target =" ^ vector_print_string b_transformed |> print_endline in *)
  let diags = diagonal_entries d in 
  (* let _ = "diagonal =" ^ vector_print_string diags |> print_endline in *)
  let n_determined = diags |> List.filter (fun x -> x <> IntPID.zero) |> List.length in
  if not (List.for_all2 (fun d_t b_t -> IntPID.divides b_t d_t) (List.take n_determined diags) (List.take n_determined b_transformed)) ||
    not (b_transformed |> List.drop n_determined |> List.for_all (fun x -> x = IntPID.zero)) then None else
      let y = (List.map2 (fun b_t d_t -> if b_t = IntPID.zero then IntPID.zero else IntPID.div b_t d_t |> Option.get) (List.take n_determined b_transformed) (List.take n_determined diags)) @
        (d |> IntMatrix.columns |> List.drop n_determined |> List.map (fun _ -> IntPID.zero)) in
      let x = [y] |> IntMatrix.of_columns |> IntMatrix.mul v |> IntMatrix.columns |> List.hd in
      let free_solutions = v |> IntMatrix.columns |> List.drop n_determined in
      Some {base_solution = x; free_vectors = free_solutions}

let adjust_pivot_sign a u t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  let pivot = List.nth col t in
  if IntPID.is_neg pivot then switch_row_sign a u t else (a, u)

let column_above_phase a u t j_t = let col = List.nth (IntMatrix.columns a) j_t in
  let pivot = List.nth col t in
  let to_adjust = col |> List.take t in
  let (a_new, u_new, _) = List.fold_left (fun (a_curr, u_curr, k) x -> if x = IntPID.zero then (a_curr, u_curr, k + 1) else
    let factor = if IntPID.divides x pivot then IntPID.div x pivot |> Option.get else 
      (let rem = IntPID.remainder x pivot in
      IntPID.div (IntPID.add x (IntPID.neg rem)) pivot |> Option.get |> IntPID.add IntPID.one)
    in
    let (a_next, u_next) = eliminate_row a_curr u_curr t k factor in
    (a_next, u_next, k + 1))
    (a, u, 0)
    to_adjust 
  in
  (a_new, u_new)
  

let rec upper_triangle_phase a u t j_t_candidate = let maybe_next_column = find_next_column_index a t j_t_candidate in
  match maybe_next_column with 
  | None -> (a, u)
  | Some j_t -> 
    let (a_pivot, u_pivot) = find_pivot a u t j_t in
    let (a_sign, u_sign) = adjust_row_signs_below a_pivot u_pivot t j_t in
    let (a_below, u_below) = column_phase a_sign u_sign t j_t in
    let (a_below_sign, u_below_sign) = adjust_pivot_sign a_below u_below t j_t in
    let (a_new, u_new) = column_above_phase a_below_sign u_below_sign t j_t in
    upper_triangle_phase a_new u_new (t + 1) (j_t + 1)

let move_row_last a u t = let (m, _n) = IntMatrix.dimensions a in
  let adjustment_entries = Seq.ints 0 |>
      Seq.take m |>
      Seq.fold_left 
        (fun entries i -> if i < t then 
            (((i,i), IntPID.one):: entries) 
          else if i < m - 1 then 
            (((i,i + 1), IntPID.one):: entries)
          else
            (((m - 1,t), IntPID.one):: entries)
          ) 
        [] 
      in
    let adjustment_matrix = IntMatrix.of_association (m, m) adjustment_entries in
    let new_u = IntMatrix.mul adjustment_matrix u in
    let new_a = IntMatrix.mul adjustment_matrix a in
    (new_a, new_u)


(* We need to move zero-filled rows to the bottom of the matrix. *)
let reorder_rows_phase a u = let a_trans = IntMatrix.transpose a in
  let rows = IntMatrix.columns a_trans in
  let (a_new, u_new, _) = List.fold_left (fun (a_curr, u_curr, t) row -> 
    if row |> List.for_all (fun x -> x = IntPID.zero) |> not then (a_curr, u_curr, t + 1) else
      let (a_next, u_next) = move_row_last a_curr u_curr t in
      (a_next, u_next, t))
    (a, u, 0)
    rows
  in
  (a_new, u_new)

(* The row hermit for of a matrix A of dimension m x n over the integers is a pair of matrices H of dimension m x n and U of dimension m x m such that
   - H = UA;
   - U is unimodular, i.e. invertible over the integers;
   - H is upper-triangular with zero-rows only after all non-zero rows;
   - the leading non-zer0 coefficients, called pivots, of later rows are strictly to the right of those of earlier rows;
   - all pivots are positive;
   - all elements below pivots are zero; and
   - all elements above pivots are non-positive with absolute value smaller than the corresponding pivot's one.
*)
let row_hermite_normal_form a = let (m, _n) = IntMatrix.dimensions a in
   let u = diagonal_matrix m in
   (* let _ = "dims = (" ^ string_of_int m ^ " x " ^ string_of_int n ^ ")" |> print_endline in *)
   let (a_tri, u_tri) = upper_triangle_phase a u 0 0 in
   reorder_rows_phase a_tri u_tri

(* The column hermite form of a matrix A of dimension m x n is the equivalent of the row hermit normal for described above, just transposed.
   In particular, it consists of a matrix H of dimension m x n and an invertible matrix V of dimensions n x n with AV = H 
   with analogous properties as above for columns instead of rows. *)
let column_hermite_normal_form a = let a_trans = IntMatrix.transpose a in
      let (h_col, u_col) = row_hermite_normal_form a_trans in
      (IntMatrix.transpose h_col, IntMatrix.transpose u_col)


let print_linear_solution {base_solution; free_vectors} = print_endline "";
  print_endline "base:"; 
  base_solution |> vector_print_string |> print_endline;
  print_endline "free vectors:";
  List.iter (fun v -> v |> vector_print_string |> print_endline) free_vectors; 
  print_endline ""

(* This exploits that we know that each free vector needs to have at least one positive and one negative entry.
   We know this, because all buttons only increase joltages and the free vectors are linearly independent. *)
let optimize_single_free_vector base_solution free_vector = let b = base_solution |> List.map IntPID.to_int in
  let f = free_vector |> List.map IntPID.to_int in
  let n_f_min = List.combine b f |> 
    List.filter (fun (_, f_t) -> f_t > 0) |>
    List.map (fun (b_t, f_t) -> if b_t >= 0 then - (b_t / f_t) else if (-b_t) mod f_t = 0 then (-b_t) / f_t else (-b_t) / f_t + 1) |>
    List.fold_left (fun acc x -> match acc with | None -> Some x | Some y -> Some (max y x)) None |>
    Option.get 
  in
  let n_f_max = List.combine b f |> 
    List.filter (fun (_, f_t) -> f_t < 0) |>
    List.map (fun (b_t, f_t) -> if b_t >= 0 then b_t / (-f_t) else if b_t mod (-f_t) = 0 then - ((-b_t) / (-f_t)) else -((-b_t) / (-f_t)) - 1) |>
    List.fold_left (fun acc x -> match acc with | None -> Some x | Some y -> Some (min y x)) None |>
    Option.get 
  in
  if n_f_max < n_f_min then (* let _ = " " ^ string_of_int n_f_min ^ " <= n_f <= " ^ string_of_int n_f_max |> print_endline in *) None else 
    (* let _ = " " ^ string_of_int n_f_min ^ " <= n_f <= " ^ string_of_int n_f_max  |> print_endline in *)
    let weight = List.fold_left Int.add 0 f in
    let x = if weight >= 0 then 
      List.map2 (fun b_t f_t -> IntPID.add b_t (IntPID.mul (IntPID.of_int n_f_min) f_t)) base_solution free_vector
    else 
      List.map2 (fun b_t f_t -> IntPID.add b_t (IntPID.mul (IntPID.of_int n_f_max) f_t)) base_solution free_vector 
    in
    (* let _ = " result =" ^ vector_print_string (List.map IntPID.of_int x) |> print_endline in *)
    Some x


(* We bring the free vectors into hermit normal form and ensure that each needs to to be added a non-negative number of times for any feasible solution. *)
(* The free vectors need to be linearly independent for thisto work. *)
let improve_base_solution_and_free_vectors base_solution free_vectors = let (f_improved, _) = free_vectors |> IntMatrix.of_columns |> column_hermite_normal_form in
    let b_improved = f_improved |> IntMatrix.columns |> List.fold_left
      (fun b_curr col -> let t = List.find_index (fun x -> x <> IntPID.zero) col |> Option.get in
        let f_t = List.nth col t |> IntPID.to_int in
        let b_t = List.nth b_curr t |> IntPID.to_int in 
        let n_f_min = if b_t >= 0 then - (b_t / f_t) else if (-b_t) mod f_t = 0 then (-b_t) / f_t else (-b_t) / f_t + 1 in
          List.map2 (fun b_i f_i -> IntPID.add b_i (IntPID.mul (IntPID.of_int n_f_min) f_i)) b_curr col
         ) base_solution in
        (b_improved, f_improved |> IntMatrix.columns)


(* This method is very inefficient, when there are many free vectors. However, for the given inputs, there are never more than three.
   Moreover, we use a further special property of our input to bound counts from above. *)
let rec optimize_by_enumerating_over_plausible_first_free_vector_counts base_solution free_vectors rotations = if List.length free_vectors = 1 then 
    optimize_single_free_vector base_solution (List.hd free_vectors) 
  else 
    let maybe_upper_bound_index = free_vectors |> IntMatrix.of_columns |> IntMatrix.transpose |> IntMatrix.columns |> List.find_index 
      (fun row -> row |> List.hd |> IntPID.is_neg && row |> List.for_all (fun x -> IntPID.is_neg x || x = IntPID.zero)) in
      match maybe_upper_bound_index with
      | None ->  
          if rotations >= List.length free_vectors then 
            let _ = print_endline "Special case handling necessary for "; print_linear_solution {base_solution; free_vectors} in 
              let new_first = List.map2 IntPID.add (List.hd free_vectors) (List.nth free_vectors 1) in
              let new_frees = new_first :: (List.drop 1 free_vectors) in
              (* There is no guarantee this will terminate; in general it will not. However, it works in the one case we run into this issue in our input. *)
              optimize_by_enumerating_over_plausible_first_free_vector_counts base_solution new_frees rotations
          else
            let rotated_free_vectors = (free_vectors |> List.drop 1) @ [List.hd free_vectors] in
            optimize_by_enumerating_over_plausible_first_free_vector_counts base_solution rotated_free_vectors (rotations + 1)
      | Some t -> 
        let first_vector = List.hd free_vectors in
        let f_t = first_vector |> (fun fs -> List.nth fs t) |> IntPID.to_int in 
        let b_t = List.nth base_solution t |> IntPID.to_int in
        let n_f_max = if b_t >= 0 then b_t / (-f_t) else if b_t mod (-f_t) = 0 then - ((-b_t) / (-f_t)) else -((-b_t) / (-f_t)) - 1 in 
        let lower_bound_index = free_vectors |> List.hd |> List.find_index  
          (fun x -> x <> IntPID.zero && x |> IntPID.is_neg |> not) |> 
        Option.get in
        let f_min_t = first_vector |> (fun fs -> List.nth fs lower_bound_index) |> IntPID.to_int in 
        let b_min_t = List.nth base_solution lower_bound_index |> IntPID.to_int in
        let n_f_min = if b_min_t >= 0 then - (b_min_t / f_min_t) else if (-b_min_t) mod f_min_t = 0 then (-b_min_t) / f_min_t else (-b_min_t) / f_min_t + 1 in 
        if n_f_max < n_f_min then None else
          let further_free_vectors = List.drop 1 free_vectors in
          let start_solution = List.map2 (fun b_t f_t -> IntPID.add b_t (IntPID.mul (IntPID.of_int n_f_min) f_t)) base_solution first_vector in
          let (best_solution, _least_weight, _) = Seq.ints n_f_min |> Seq.take (n_f_max - n_f_min + 1) |> Seq.fold_left
          (fun (maybe_best_solution_curr, maybe_least_weight_curr, start_solution_n) _ -> 
            let start_solution_next = List.map2 IntPID.add start_solution_n first_vector in 
            let maybe_optimized_n = optimize_by_enumerating_over_plausible_first_free_vector_counts start_solution_n further_free_vectors 0 in
            match maybe_optimized_n with
            | None -> (maybe_best_solution_curr, maybe_least_weight_curr, start_solution_next)
            | Some optimized_n ->
                if optimized_n |> List.exists IntPID.is_neg then 
                  (* The start solution cannot be made feasible for an index that is no longer chaged after the current free vector. *)
                  (maybe_best_solution_curr, maybe_least_weight_curr, start_solution_next) 
                else
                  let weight_n = optimized_n |> List.fold_left IntPID.add IntPID.zero |> IntPID.to_int in
                  match maybe_least_weight_curr with
                  | None -> (Some optimized_n, Some weight_n, start_solution_next)
                  | Some least_weight_curr -> if weight_n < least_weight_curr then 
                      (Some optimized_n, Some weight_n, start_solution_next) 
                    else
                      (maybe_best_solution_curr, maybe_least_weight_curr, start_solution_next)

            )
          (None, None, start_solution) in
          best_solution



let optimize_free_vectors base_solution free_vectors = let maybe_optimal_solution = if List.length free_vectors = 1 then 
    optimize_single_free_vector base_solution (List.hd free_vectors) 
  else
    (* let _ = print_endline "before improvement"; print_linear_solution {base_solution; free_vectors} in *)
    let (b_improved, f_improved) = improve_base_solution_and_free_vectors base_solution free_vectors in
    (* This is now a standard integer program with the matric A given by minus the free vectors, b by the base solution and c by minus the totals of the free vectors. *)
    (* let _ = (* print_endline "after improvement"; *) print_linear_solution {base_solution = b_improved; free_vectors = f_improved} in *)
    (* Implementing a full general linear program solver here is probably a bit much. So, I use something that works sufficiently fast given the low remaining number of free vectors in the problem input. *)
    optimize_by_enumerating_over_plausible_first_free_vector_counts b_improved f_improved 0
  in
  (* let _ = match maybe_optimal_solution with | None -> print_endline "No solution!" | Some solution -> print_endline "solution:"; solution |> vector_print_string |> print_endline in *)
  let _ = if Option.is_none maybe_optimal_solution then (print_linear_solution {base_solution; free_vectors}; print_endline "No solution!") in
  maybe_optimal_solution


let joltage_requirement_print_string values = let total = List.fold_left (fun acc x -> acc + x) 0 values in
  "   {" ^ String.concat "," (values |> List.map string_of_int) ^ "} with total = " ^ string_of_int total

let solve_via_linear_algebra buttons joltage_requirement = let button_matrix = buttons |>
  List.map (fun col -> col |> List.map IntPID.of_int) |>
  IntMatrix.of_columns in
  let target = joltage_requirement |> List.map IntPID.of_int in
    (* let _ = "target =" ^ vector_print_string target |> print_endline in *)
    let maybe_unconstrained_solution = solve_via_smith_normal_form button_matrix target in
    match maybe_unconstrained_solution with
    | None -> let _ = "Inconsistent system for joltage requirement" ^ (joltage_requirement |> joltage_requirement_print_string) |> print_endline in 
        None
    | Some {base_solution; free_vectors} ->
      if List.length free_vectors = 0 then
        if List.exists IntPID.is_neg base_solution then 
          let _ = "Infeasible solution " ^ (base_solution |> vector_print_string) ^ " for joltage requirement" ^ (joltage_requirement |> joltage_requirement_print_string) |> print_endline in 
            None 
        else 
          Some base_solution
      else (* The underdetermined case is trickier, since an actual optimization is required.*)
        let maybe_solution = optimize_free_vectors base_solution free_vectors in
        let _ = if Option.is_none maybe_solution then "No solution for joltage requirement" ^ (joltage_requirement |> joltage_requirement_print_string) |> print_endline in
        maybe_solution
 
          
let least_presses_required_to_power machine = let {indicators = _; buttons; joltage_requirement} = machine in
  solve_via_linear_algebra buttons joltage_requirement |>
  Option.map (List.fold_left IntPID.add IntPID.zero) |>
  Option.map IntPID.to_int
    
let solve_part_2 machines = machines |>
          List.map least_presses_required_to_power |>
          List.map Option.get |>
          List.fold_left Int.add 0 |>
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



let example_input_part_1 = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
"

let%test "part 2 works for example part 1" = let expectation = "10" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 1"; print_endline "" in *)
  let input = parse example_input_part_1 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on example part 1 is " ^ actual);
  actual = expectation


let example_input_part_2 = "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
"

let%test "part 2 works for example part 2" = let expectation = "12" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 2"; print_endline "" in *)
  let input = parse example_input_part_2 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on example part 2 is " ^ actual);
  actual = expectation


let example_input_part_3 = "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"

let%test "part 2 works for example part 3" = let expectation = "11" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 3"; print_endline "" in *)
  let input = parse example_input_part_3 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on example part 3 is " ^ actual);
  actual = expectation


let special_example_1 = "[..#..#..##] (2,3,5,6,8) (1,3,4,5,6,7,9) (2,4,6,9) (1,2,5,7,8) (1,3,4,6,7,8,9) (1,2,6,8) (2,7) (2,3,9) (0,1,2,3,4,6,7,8,9) (0,2,5,7) (1,6,8) {36,54,79,42,42,41,63,75,43,49}
"
let%test "part 2 works for special example 1" = let expectation = "111" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 3"; print_endline "" in *)
  let input = parse special_example_1 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on special example 1 is " ^ actual);
  actual = expectation


let special_example_2 = "[#####....] (0,2,3,7,8) (0,1,2,3,4,6,7,8) (1,2,3,4,5,6,8) (0,3,4,5,6,7,8) (0,1,3,5) (0,4,5,6,8) (2,6) (0,1,3,4,6) (0,3,4) (1,2,4,5,6,7) (4,6) {55,45,33,58,75,56,71,31,36}
"
let%test "part 2 works for special example 2" = let expectation = "89" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 3"; print_endline "" in *)
  let input = parse special_example_2 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on special example 2 is " ^ actual);
  actual = expectation


let special_example_3 = "[#.##..####] (0,1,2,4,6,9) (1,3,4,5,6,7,8,9) (2,7) (0,2,3,4,5,6) (0,1,3,4,5,8,9) (0,1,2,4,6,8) (4,5,7,8) (1,2,5) (1,4,8) (0,1,2,3,5,6,7,8,9) (1,3,5) {45,77,57,42,73,60,57,46,62,46}
"

let%test "part 2 works for special example 3" = let expectation = "102" in
  (* let _ = print_endline ""; print_endline ""; print_endline "testing example 3"; print_endline "" in *)
  let input = parse special_example_3 |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on special example 3 is " ^ actual);
  actual = expectation


let%test "part 2 works for example" = let expectation = "33" in
  let input = parse example_input |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on example is " ^ actual);
  actual = expectation


let%test "part 2 works for real input" = let expectation = "16361" in
  let input_text = InputReader.read_day_input 10 in
  let input = parse input_text |> Option.get in
  let actual = solve_part_2 input in
  if not (actual = expectation) then print_endline ("Computed solution for part 2 of day 10 on real inputs is " ^ actual);
  actual = expectation
