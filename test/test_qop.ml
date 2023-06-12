open Lqccs.Qop
open Lqccs.Eval

(** Returns a string representing the passed list of complex *)
let list2string l =
  let p1 = Printf.sprintf "[" in
  let els = List.map (fun el -> Printf.sprintf "%f %f" el.Complex.re el.Complex.im) l |> String.concat " " in
  let p2 = Printf.sprintf "]\n" in
  p1 ^ els ^ p2

(** Test the behaviour of a unary quantum gate against the respected result and print the outcome *)
let assert_unary_gate_res gate q_state qbit_index expected_res ind =
  try
    let res = gate q_state qbit_index in
    if res = expected_res then
      Printf.printf "[ OK ] Test #%2d\n" ind
    else
      Printf.printf "[FAIL] Test #%2d:\n\tExpected:%s\tGot:%s\n" ind (list2string expected_res) (list2string res)
  with
    | Lqccs.Qop.QuantumException msg -> Printf.printf "[ FAIL ] Test #%2d: %s\n" ind msg

(** Test the behaviour of a unary quantum gate by making sure it raises an exception*)
let assert_unary_gate_except gate q_state qbit_index ind =
  try
    let _ = gate q_state qbit_index in
    Printf.printf "[FAIL] Test #%2d should fail but doesn't\n" ind
  with
    | Lqccs.Qop.QuantumException msg ->  Printf.printf "[ OK ] Test #%2d: %s\n" ind msg

(** Test the behaviour of the CX gate against the respected result and print the outcome *)
let assert_cx_res q_state control_q target_q expected_res ind =
  try
    let res = qop_cx q_state control_q target_q in
    if res = expected_res then
      Printf.printf "[ OK ] Test #%2d\n" ind
    else
      Printf.printf "[FAIL] Test #%2d:\n\tExpected:%s\tGot:%s\n" ind (list2string expected_res) (list2string res)
  with
    | Lqccs.Qop.QuantumException msg -> Printf.printf "[FAIL] Test #%2d: %s\n" ind msg

let rec string_of_conf_qop (clist : (Complex.t list * int * float) list) = match clist with
 | [] -> ""
 | (qst, v, prob)::xs -> Printf.sprintf "(%s, %s, %s) %s" (string_of_qstate qst) (string_of_int v) (string_of_float prob) (string_of_conf_qop xs)

(** Test the behaviour of the measurement against the respected result and print the outcome *)
let assert_measure_res q_state qbits expected_res ind =
  try
    let res = measure q_state qbits in
    if res = expected_res then
      Printf.printf "[ OK ] Test #%2d\n" ind
    else
      Printf.printf "[FAIL] Test #%2d:\n\tExpected: %s\n\tGot: %s\n" ind (string_of_conf_qop expected_res) (string_of_conf_qop res)
  with
    | Lqccs.Qop.QuantumException msg -> Printf.printf "[FAIL] Test #%2d: %s\n" ind msg

(** List of tests to perform *)
let tests =
  [
    (* HADAMARD gate *)
    assert_unary_gate_res qop_h [Complex.one; Complex.zero] 1 [Complex.{re = 1. /. Float.sqrt 2.; im = 0.}; Complex.{re = 1. /. Float.sqrt 2.; im = 0.}];
    assert_unary_gate_except qop_h [Complex.one; Complex.zero] 2;
    (* X gate *)
    assert_unary_gate_res qop_x [Complex.one; Complex.zero] 1 [Complex.zero; Complex.one];
    assert_unary_gate_except qop_x [Complex.one; Complex.zero] 2;
    (* Z gate *)
    assert_unary_gate_res qop_z [Complex.one; Complex.zero] 1 [Complex.one; Complex.zero];
    assert_unary_gate_except qop_z [Complex.one; Complex.zero] 2;
    (* Y gate *)
    assert_unary_gate_res qop_y [Complex.one; Complex.zero] 1 [Complex.zero; Complex.i];
    assert_unary_gate_except qop_y [Complex.one; Complex.zero] 2;
    (* CX gate *)
    assert_cx_res [Complex.zero; Complex.zero; Complex.one; Complex.zero] 1 2 [Complex.zero; Complex.zero; Complex.zero; Complex.one];
    assert_cx_res [Complex.one; Complex.zero; Complex.zero; Complex.zero] 1 2 [Complex.one; Complex.zero; Complex.zero; Complex.zero];
    (* Measure *)
    assert_measure_res [Complex.one; Complex.zero] [1] [([Complex.one; Complex.zero], 0, 1.0)];
    assert_measure_res [Complex.one; Complex.zero; Complex.zero; Complex.zero] [1;2] [([Complex.one; Complex.zero; Complex.zero; Complex.zero], 0, 1.0)];
    assert_measure_res [Complex.{re = 1. /. Float.sqrt 2.; im = 0.}; Complex.{re = 1. /. Float.sqrt 2.; im = 0.}] [1] [([Complex.one; Complex.zero], 0, 0.5); ([Complex.zero; Complex.one], 1, 0.5)];
  ]

(** Perform tests *)
let _ =
  Printf.printf "\n--- Quantum operations ---\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests