open Lqccs.Qop

(** Returns a string representing the passed float list *)
let list2string l =
  let p1 = Printf.sprintf "[" in
  let els = List.map (fun el -> Printf.sprintf "%f" el) l |> String.concat " " in
  let p2 = Printf.sprintf "]\n" in
  p1 ^ els ^ p2


(** Test the behaviour of a unary quantum gate against the respected result and print the outcome *)
let assert_gate_res gate q_state qbit_index expected_res ind =
  try
    let res = gate q_state qbit_index in
    if res = expected_res then
      Printf.printf "[ OK ] Test #%2d\n" ind
    else
      Printf.printf "[ FAIL ] Test #%2d:\n\tExpected:%s\tGot:%s\n" ind (list2string expected_res) (list2string res)
  with
    | Lqccs.Qop.QuantumException msg -> Printf.printf "[ FAIL ] Test #%2d: %s\n" ind msg

(** Test the behaviour of a unary quantum gate by making sure it raises an exception*)
let assert_gate_except gate q_state qbit_index ind =
  try
    let _ = gate q_state qbit_index in
    Printf.printf "[ FAIL ] Test #%2d should fail but doesn't\n" ind
  with
    | Lqccs.Qop.QuantumException msg ->  Printf.printf "[ OK ] Test #%2d: %s\n" ind msg

(** List of tests to perform *)
let tests =
  [
    (* HADAMARD gate *)
    assert_gate_res qop_h [1.; 0.] 1 [1. /. sqrt 2.; 1. /. sqrt 2.];
    assert_gate_except qop_h [1.; 0.] 2;
    (* X gate *)
    assert_gate_res qop_x [1.; 0.] 1 [0.; 1.];
    assert_gate_except qop_x [1.; 0.] 2;
    (* Z gate *)
    assert_gate_res qop_z [1.; 0.] 1 [1.; 0.];
    assert_gate_except qop_z [1.; 0.] 2;
    (* CX gate *)
    (* TODO: exhaustive test of all gates and the measure operation *)
  ]

(** Perform tests *)
let _ =
  Printf.printf "\n--- Quantum operations ---\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests