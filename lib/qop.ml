open Complex

module Mat = Owl.Dense.Matrix.Z

exception QuantumException of string

type measurement_result = {quantum_state: t list; value: int list; probability: float}

let debug = false

(** Print the contents of a matrix if we are in debug mode *)
let mat_debug mat = if debug then Mat.print mat else ()

(** Round a float to two significant digits *)
let round2 n = Float.round (n *. 100.) /. 100.

(** Returns the 2x2 identity matrix  *)
let make_id : Mat.mat =
  let id = Mat.empty 2 2 in
    Mat.set id 0 0 {re = 1.0; im = 0.0}; Mat.set id 0 1 {re = 0.0; im = 0.0};
    Mat.set id 1 0 {re = 0.0; im = 0.0}; Mat.set id 1 1 {re = 1.0; im = 0.0};
  id

(** Returns the 4x4 swap matrix *)
let make_swap : Mat.mat = 
  let swap = Mat.empty 4 4 in
    Mat.set swap 0 0 {re = 1.0; im = 0.0}; Mat.set swap 0 1 {re = 0.0; im = 0.0}; Mat.set swap 0 2 {re = 0.0; im = 0.0}; Mat.set swap 0 3 {re = 0.0; im = 0.0};
    Mat.set swap 1 0 {re = 0.0; im = 0.0}; Mat.set swap 1 1 {re = 0.0; im = 0.0}; Mat.set swap 1 2 {re = 1.0; im = 0.0}; Mat.set swap 1 3 {re = 0.0; im = 0.0};
    Mat.set swap 2 0 {re = 0.0; im = 0.0}; Mat.set swap 2 1 {re = 1.0; im = 0.0}; Mat.set swap 2 2 {re = 0.0; im = 0.0}; Mat.set swap 2 3 {re = 0.0; im = 0.0};
    Mat.set swap 3 0 {re = 0.0; im = 0.0}; Mat.set swap 3 1 {re = 0.0; im = 0.0}; Mat.set swap 3 2 {re = 0.0; im = 0.0}; Mat.set swap 3 3 {re = 1.0; im = 0.0};
  swap

(** Return the normalized vector *)
let normalize (values : t list) : t list =
  let vec_length = sqrt (List.fold_left (fun acc el -> add acc (mul el el)) zero values) in
  if vec_length > zero then
    List.map (fun el -> div el vec_length) values
  else
    values

(** Returns a vector representing the input list *)
let list2vec (lst : t list) : Mat.mat =
  let n = List.length lst in
  let vec = Mat.empty n 1 in
    List.iteri (fun i x -> Mat.set vec i 0 x) lst;
  vec

(** Returns a list representing the input vector *)
let vec2list (vec : Mat.mat) : t list = 
  Mat.fold_rows (fun lst row -> (Mat.get row 0 0)::lst) [] vec |> List.rev 

(** Perform the cartesian product of the provided lists *)
let rec cart_prod currlis lists =
  match lists with
  | [] -> [ List.rev currlis ]
  | first_list :: other_lists ->
  List.fold_left
  (fun acc elem ->
  let this_res = cart_prod (elem :: currlis) other_lists in
  List.append this_res acc)
  [] (List.rev first_list)

(** TODO *)
let generate_cases (q_state_len : int) (to_measure : int list) =
  let ops_list = List.init q_state_len (fun ind ->
    let rec get_index v lst curr =
      match lst with 
      | [] -> -1
      | x::xs -> if v = x then curr else get_index v xs (curr + 1)
    in
    (* Get the position of each qbit in the order they were passed to the measurement function *)
    let rank = get_index (ind + 1) to_measure 0 in 
    if rank != -1 then
      (* We are supposed to measure this qbit *)
      [(rank, 0); (rank, 1)]
    else 
      (* We are not supposed to measure this qbit *)
      [(rank, -1)]) 
  in
  cart_prod [] ops_list

(** Return the appropriate matrix based on the requested operation i,
      0: M0
      1: M1
     -1: ID *)
let select_measurement_mat i =
  if i = -1 then make_id
  else if i = 0 then 
    let m0 = Mat.empty 2 2 in
      Mat.set m0 0 0 {re = 1.0; im = 0.0}; Mat.set m0 0 1 {re = 0.0; im = 0.0};
      Mat.set m0 1 0 {re = 0.0; im = 0.0}; Mat.set m0 1 1 {re = 0.0; im = 0.0};
      m0
  else if i = 1 then
    let m1 = Mat.empty 2 2 in
      Mat.set m1 0 0 {re = 0.0; im = 0.0}; Mat.set m1 0 1 {re = 0.0; im = 0.0};
      Mat.set m1 1 0 {re = 0.0; im = 0.0}; Mat.set m1 1 1 {re = 1.0; im = 0.0};
      m1
  else
    failwith "The value of 'i' must be -1, 0 or 1."

(** Returns the result of applying the specified measurement.
    ops is expected to be a list of numbers of the following type:
      0: measure with M0
      1: measure with M1
      -1: apply id *)
let apply_measurement (q_state_vec : Mat.mat) (ops : (int * int) list) =
  (* Auxiliary function to perform tensor products *)
  let rec create_matrix lst acc =
    match lst with 
      | [] -> acc
      | hd::rest -> let m = select_measurement_mat hd in
          let acc = Mat.kron acc m in
          create_matrix rest acc 
  in
    match ops with
    | [] -> raise (QuantumException "Nothing to measure")
    | (_, op_id)::rest -> let initial_mat = select_measurement_mat op_id in 
      let complete_mat = create_matrix (List.map snd rest) initial_mat in
      let meas_mat = Mat.dot (Mat.ctranspose complete_mat) complete_mat in
      let prob = Mat.get (Mat.dot (Mat.dot (Mat.ctranspose q_state_vec) meas_mat) q_state_vec) 0 0 in
      let new_state = if prob == {re = 0.0; im = 0.0} then 
          Mat.zeros (Mat.col_num q_state_vec) 1
        else
          let _ = mat_debug meas_mat in
          Mat.dot (Mat.mul_scalar meas_mat (sqrt prob)) q_state_vec in
      (* Extract the bits *)
      let v = List.filter (fun (_, op) -> (op = 0) || op = 1) ops
      (* Sort them to reflect the order in which qbits were passed to measure *)
        |> List.sort (fun (rankA, _) (rankB, _)-> Stdlib.compare rankA rankB) 
      (* Drop the rank associated with each bit *)
        |> List.map snd in
      mat_debug new_state;
      {quantum_state = new_state |> vec2list |> normalize; value = v; probability = prob.re |> round2} 

let binary_to_decimal binary =
  let rec binary_to_decimal_helper binary power acc =
    match binary with
    | [] -> acc
    | bit :: rest ->
      let decimal_value = bit * int_of_float (2. ** float_of_int power) in
      binary_to_decimal_helper rest (power - 1) (acc + decimal_value)
  in
  binary_to_decimal_helper binary (List.length binary - 1) 0

(** Returns the list of all the possible outcomes obtainable from the measurement of the provided list of qbits *) 
let measure (q_state : t list) (qbits : int list) : (t list * int * float) list =
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  let q_state_vec = list2vec q_state in
  let cases = generate_cases state_length qbits in
  let results = List.map (fun case ->
    let {quantum_state; value; probability} = apply_measurement q_state_vec case in
    (quantum_state, (binary_to_decimal value), probability)
  ) cases in
  List.filter (fun el -> match el with 
    | (_, _, prob) when prob = 0.0 -> false 
    | _ -> true
  ) results

(** Swaps the ith (i+1)th qbits in place *)
let step_forwards (q_state_vec : Mat.mat) (i : int) : Mat.mat =
  let state_length = float_of_int (Mat.row_num q_state_vec) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  if i == state_length then
    (* No swaps to do *)
    q_state_vec
  else
    let id = make_id in
    let swap = make_swap in
    (* Create the matrix which applies swap to the ith and (i+1)th qbits and the identity to all the others *)
    let rec create_matrix m_tmp j =
      if j > state_length then
        m_tmp
      else
        if j = i then 
          let new_m = Mat.kron m_tmp swap in
            (* Skip the next qbit as it is the one we just swapped *)
            create_matrix new_m (j + 2)
        else
          let new_m = Mat.kron m_tmp id in
            create_matrix new_m (j + 1)
    in 
      (* Compute the resulting quantum state *)
      let (initial_m, initial_index) = if i = 1 then (swap, 3) else (id, 2) in
      let m = create_matrix initial_m initial_index in
        Mat.dot m q_state_vec 

(** Swaps the ith (i-1)th qbits in place *)
let step_backwards (q_state_vec : Mat.mat) (i : int) : Mat.mat  =
  let state_length = float_of_int (Mat.row_num q_state_vec) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  if i == 1 then
    (* No swaps to do *)
    q_state_vec
  else
    let id = make_id in
    let swap = make_swap in
    (* Create the matrix which applies swap to the ith and (i-1)th qbits and the identity to all the others *)
    let rec create_matrix m_tmp j =
      if j > state_length then
        m_tmp
      else
        if j = i-1 then 
          let new_m = Mat.kron m_tmp swap in
            (* Skip the next qbit as it is the one we just swapped *)
            create_matrix new_m (j + 2)
        else
          let new_m = Mat.kron m_tmp id in
          create_matrix new_m (j + 1)
    in 
      (* Compute the resulting quantum state *)
      let (initial_m, initial_index) = if i = 1 then (swap, 3) else (id, 2) in
      let m = create_matrix initial_m initial_index in
        Mat.dot m q_state_vec

(* Push the qbit at qbit_index forwards or backwards based on incr until it reaches the desired boundary *)
let rec push state qbit_index boundary incr =
  if qbit_index = boundary then
    state
  else
    let op = if incr > 0 then step_forwards else step_backwards in
    let new_state = op state qbit_index in
    push new_state (qbit_index + incr) boundary incr
    
(** Returns the new quantum state obtained by applying the Hadamard gate to the ith qbit *)
let qop_h (q_state : t list) (i : int) : t list = 
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  let id = make_id in
  let h = Mat.empty 2 2 in
    Mat.set h 0 0 {re = 1.0 /. Float.sqrt 2.0; im = 0.0}; Mat.set h 0 1 {re = 1.0 /. Float.sqrt 2.0; im = 0.0};
    Mat.set h 1 0 {re = 1.0 /. Float.sqrt 2.0; im = 0.0}; Mat.set h 1 1 {re = -1.0 /. Float.sqrt 2.0; im = 0.0};
  let q_state_vec = list2vec q_state in
  (* Create the matrix which applies Hadamard to the ith qbit and the identity to all the others *)
  let rec create_matrix m_tmp j =
    if j > state_length then
      m_tmp
    else
      if j = i then 
        let new_m = Mat.kron m_tmp h in
          create_matrix new_m (j + 1)
      else
        let new_m = Mat.kron m_tmp id in
        create_matrix new_m (j + 1)
  in 
    (* Compute the resulting quantum state *)
    let initial_m = if i = 1 then h else id in
    let m = create_matrix initial_m 2 in
    Mat.dot m q_state_vec |> vec2list
      
(** Returns the new quantum state obtained by applying the X gate to the ith qbit *)
let qop_x (q_state : t list) (i : int) : t list =
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  let id = make_id in
  let x = Mat.empty 2 2 in
    Mat.set x 0 0 {re = 0.0; im = 0.0}; Mat.set x 0 1 {re = 1.0; im = 0.0};
    Mat.set x 1 0 {re = 1.0; im = 0.0}; Mat.set x 1 1 {re = 0.0; im = 0.0};
  let q_state_vec = list2vec q_state in
  (* Create the matrix which applies Not to the ith qbit and the identity to all the others *)
  let rec create_matrix m_tmp j =
    if j > state_length then
      m_tmp
    else
      if j = i then 
        let new_m = Mat.kron m_tmp x in
          create_matrix new_m (j + 1)
      else
        let new_m = Mat.kron m_tmp id in
        create_matrix new_m (j + 1)
  in 
    (* Compute the resulting quantum state *)
    let initial_m = if i = 1 then x else id in
    let m = create_matrix initial_m 2 in
    Mat.dot m q_state_vec |> vec2list
    
(** Returns the new quantum state obtained by applying the Z gate to the ith qbit *)
let qop_z (q_state : t list) (i : int) : t list =
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  let id = make_id in
  let z = Mat.empty 2 2 in
    Mat.set z 0 0 {re = 1.0; im = 0.0}; Mat.set z 0 1 {re = 0.0; im = 0.0};
    Mat.set z 1 0 {re = 0.0; im = 0.0}; Mat.set z 1 1 {re = -1.0; im = 0.0};
  let q_state_vec = list2vec q_state in
  (* Create the matrix which applies Not to the ith qbit and the identity to all the others *)
  let rec create_matrix m_tmp j =
    if j > state_length then
      m_tmp
    else
      if j = i then 
        let new_m = Mat.kron m_tmp z in
          create_matrix new_m (j + 1)
      else
        let new_m = Mat.kron m_tmp id in
        create_matrix new_m (j + 1)
  in 
    (* Compute the resulting quantum state *)
    let initial_m = if i = 1 then z else id in
    let m = create_matrix initial_m 2 in
    Mat.dot m q_state_vec |> vec2list

(** Returns the new quantum state obtained by applying the Y gate to the ith qbit *)
let qop_y (q_state : t list) (i : int) : t list =
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  if i < 1 || i > state_length then 
    raise (QuantumException "Index out of bounds");
  let id = make_id in
  let y = Mat.empty 2 2 in
    Mat.set y 0 0 {re = 0.0; im = 0.0}; Mat.set y 0 1 {re = 0.0; im = -1.0};
    Mat.set y 1 0 {re = 0.0; im = 1.0}; Mat.set y 1 1 {re = 0.0; im = 0.0};
  let q_state_vec = list2vec q_state in
  (* Create the matrix which applies Not to the ith qbit and the identity to all the others *)
  let rec create_matrix m_tmp j =
    if j > state_length then
      m_tmp
    else
      if j = i then 
        let new_m = Mat.kron m_tmp y in
          create_matrix new_m (j + 1)
      else
        let new_m = Mat.kron m_tmp id in
        create_matrix new_m (j + 1)
  in 
    (* Compute the resulting quantum state *)
    let initial_m = if i = 1 then y else id in
    let m = create_matrix initial_m 2 in
    Mat.dot m q_state_vec |> vec2list

(** Returns the new quantum state obtained by applying the CNOT gate to the ith and jth qbits,
    where i is the control qbit and j is the target qbit *)
let qop_cx (q_state : t list) (control_q : int) (target_q : int) : t list =
  if control_q = target_q then raise (QuantumException "The control and target qbits must be distinct");
  let state_length = float_of_int (List.length q_state) |> Float.log2 |> Float.floor |> int_of_float in
  if control_q < 1 || control_q > state_length || target_q < 1 || target_q > state_length then 
    raise (QuantumException "Index out of bounds");
  let id = make_id in
  let cx = Mat.empty 4 4 in
    Mat.set cx 0 0 {re = 1.0; im = 0.0}; Mat.set cx 0 1 {re = 0.0; im = 0.0}; Mat.set cx 0 2 {re = 0.0; im = 0.0}; Mat.set cx 0 3 {re = 0.0; im = 0.0};
    Mat.set cx 1 0 {re = 0.0; im = 0.0}; Mat.set cx 1 1 {re = 1.0; im = 0.0}; Mat.set cx 1 2 {re = 0.0; im = 0.0}; Mat.set cx 1 3 {re = 0.0; im = 0.0};
    Mat.set cx 2 0 {re = 0.0; im = 0.0}; Mat.set cx 2 1 {re = 0.0; im = 0.0}; Mat.set cx 2 2 {re = 0.0; im = 0.0}; Mat.set cx 2 3 {re = 1.0; im = 0.0};
    Mat.set cx 3 0 {re = 0.0; im = 0.0}; Mat.set cx 3 1 {re = 0.0; im = 0.0}; Mat.set cx 3 2 {re = 1.0; im = 0.0}; Mat.set cx 3 3 {re = 0.0; im = 0.0};
  let q_state_vec = list2vec q_state in
  (* Create the matrix which applies CNot to the ith and jth qbits and the identity to all the others *)
  let rec create_matrix m_tmp j =
    if j > state_length then
      m_tmp
    else
      (* We assume that the control and target qbits are adjacent *)
      if j = control_q then 
        let new_m = Mat.kron m_tmp cx in
          (* Skip the next qbit since we assume it is target_q *)
          create_matrix new_m (j + 2)
      else
        let new_m = Mat.kron m_tmp id in
        create_matrix new_m (j + 1)
  in 
  (* Perform swaps until the control and target qbits are adjacent *)
  let swapped_state =
    if control_q < target_q then
      (* We need to push target_q backwards *)
      push q_state_vec target_q (control_q + 1) (-1)
    else 
      (* We need to push target_q forwards *)
      push q_state_vec target_q (control_q - 1) 1
  in
    (* Compute the resulting quantum state *)
    let first_qbit = if control_q < target_q then control_q else target_q in
    let (initial_m, initial_index) = if first_qbit = 1 then (cx, 3) else (id, 2) in
    let m = create_matrix initial_m initial_index in
    let new_state = Mat.dot m swapped_state in 
    (* Perform reverse swaps to bring the control qbit to its original position *)
    let restored_state = 
      if control_q < target_q then
        (* We need to push target_q forwards *)
        push new_state (control_q + 1) target_q 1
      else
        (* We need to push target_q backwards *)
        push new_state (control_q - 1) target_q (-1) 
    in
      vec2list restored_state

let qop_i (q_state : t list) _ = q_state