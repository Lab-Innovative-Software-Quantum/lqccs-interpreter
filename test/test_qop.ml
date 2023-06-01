open Lqccs.Qop

let test_unary_gate gate q_state qbit_index expected_res ind =
  let res = gate q_state qbit_index in
  if res = expected_res then
    Printf.printf "[ OK ] Test #%2d\n" ind
  else
    Printf.printf "[ FAIL ] Test #%2d\n" ind

let tests =
  [
    (* HADAMARD gate *)
    test_unary_gate qop_h [1.; 0.] 0 [1. /. sqrt 2.; 1. /. sqrt 2.];
    (* NOT gate *)
    test_unary_gate qop_x [1.; 0.] 0 [0.; 1.];
    (* TODO: exhaustive test of all gates and the measure operation *)
  ]

let _ =
  Printf.printf "\n--- Quantum operations ---\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests