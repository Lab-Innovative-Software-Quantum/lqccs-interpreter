open Lqccs
open Lqccs.String_of
open Lqccs.Eval

exception WrongQstate of conf

exception WrongAst of conf

exception WrongProb of conf

let assertResult source expected_results ind =
  try
    let lexbuf = Lexing.from_string ~with_positions:true source in
    let ast = Parsing.parse Scanner.next_token lexbuf in
    let ast, maxqbit = Typechecker.typecheck ast in
    let distrlist = Eval.eval ast maxqbit in
    List.iter2
      (fun (Distribution conflist) expected_conflist ->
        List.iter2
          (fun (Conf (qst, ast_res, prob))
               (expected_qst, expected_ast_str, expected_prob) ->
            let ast_res_str = string_of_program ast_res in
            if qst <> expected_qst then
              raise (WrongQstate (Conf (qst, ast_res, prob)));
            if expected_ast_str <> ast_res_str then
              raise (WrongAst (Conf (qst, ast_res, prob)));
            if prob <> expected_prob then
              raise (WrongProb (Conf (qst, ast_res, prob))))
          conflist expected_conflist)
      distrlist expected_results;
    Printf.printf "[ OK ] Test #%2d\n" ind
  with
  | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
      let header = Printf.sprintf "[FAIL] Test #%2d" ind in
      Lqccs.Errors.report_singleline header source pos msg;
      Stdlib.flush_all ()
  | Typechecker.TypeException (msg, pos) ->
      let header = Printf.sprintf "[FAIL] Test #%2d" ind in
      Lqccs.Errors.report_multiline header source pos msg;
      Stdlib.flush_all ()
  | Eval.EvalException (pos, msg) ->
      let header = Printf.sprintf "[FAIL] Test #%2d" ind in
      Errors.report_multiline header source pos msg;
      Stdlib.flush_all ()
  | WrongQstate given_conf ->
      Printf.printf "[FAIL] Test #%2d: bad quantum state in conf %s\n" ind
        (string_of_conf given_conf)
  | WrongAst given_conf ->
      Printf.printf "[FAIL] Test #%2d: bad ast in conf %s\n" ind
        (string_of_conf given_conf)
  | WrongProb given_conf ->
      Printf.printf "[FAIL] Test #%2d: bad probability in conf %s\n" ind
        (string_of_conf given_conf)
  | Invalid_argument _ ->
      Printf.printf "[FAIL] Test #%2d: result lists have different length\n" ind

let tests =
  [
    (* 1- Discard a variable *)
    assertResult "M(q1 > x).Discard(q1) \\ ()"
      [
        [ ([ 3.0; 2.0 ], "Discard(q1)", 0.5); ([ 2.0; 1.0 ], "Discard(q1)", 0.5)];
      ];
    (* 2- Empty discard after a tau *)
    assertResult "Tau.Discard() \\ ()" [ [ ([ 1.0 ], "Discard()", 1.0) ] ];
    (* 3- Parallel after measurement *)
    assertResult "M(q1 > x).Discard(q1) || Discard() \\ ()"
      [
        [ ([ 3.0; 2.0 ], "Discard(q1) || Discard()", 0.5);
          ([ 2.0; 1.0 ], "Discard(q1) || Discard()", 0.5)];
      ];
    (* 4- Choice after measurement *)
    assertResult "(M(q1 > x).(Discard(q1) ++ c:quant!q1)) \\ ()"
      [
        [ ([ 3.0; 2.0 ], "c:quant!q1", 0.5); ([ 2.0; 1.0 ], "c:quant!q1", 0.5) ];
        [ ([ 3.0; 2.0 ], "c:quant!q1", 0.5); ([ 2.0; 1.0 ], "Discard(q1)", 0.5)];
        [ ([ 3.0; 2.0 ], "Discard(q1)", 0.5); ([ 2.0; 1.0 ], "c:quant!q1", 0.5)];
        [ ([ 3.0; 2.0 ], "Discard(q1)", 0.5); ([ 2.0; 1.0 ], "Discard(q1)", 0.5)];
      ];
    (* 5- Choice with send *)
    assertResult "(Discard(q1) ++ c:quant!q1) \\ ()"
      [
        [ ([ 1.0; 0.0 ], "c:quant!q1", 1.0) ];
        [ ([ 1.0; 0.0 ], "Discard(q1)", 1.0) ];
      ];
    (* 6- If after tau *)
    assertResult "Tau.if 0 = 0 then Discard() else c:int!5 \\ ()" [[([1.0], "Discard()", 1.0)]];
    (* 7- Parallel of choices *)
    assertResult "Tau.Discard(q2) ++ Tau.Discard(q2) || Tau.Discard(q1) ++ Tau.Discard(q1) \\ ()"
      [
        [([1.0; 0.0; 0.0; 0.0], "Discard(q1) || Discard(q2)", 1.0)];
        [([1.0; 0.0; 0.0; 0.0], "Discard(q1) || Discard(q2)", 1.0)];
        [([1.0; 0.0; 0.0; 0.0], "Discard(q1) || Discard(q2)", 1.0)];
        [([1.0; 0.0; 0.0; 0.0], "Discard(q1) || Discard(q2)", 1.0)];
      ];
    (* 8- Measure and choice with if *)
    assertResult "M(q1 > x).(Discard(q1) ++ Tau.if x = 0 then Tau.Discard(q1) else Discard(q1)) \\ ()"
      [
        [([3.0; 2.0], "Discard(q1)", 0.5); ([2.0; 1.0], "Discard(q1)", 0.5)];
        [([3.0; 2.0], "Discard(q1)", 0.5); ([2.0; 1.0], "Discard(q1)", 0.5)];
        [([3.0; 2.0], "Discard(q1)", 0.5); ([2.0; 1.0], "Discard(q1)", 0.5)];
        [([3.0; 2.0], "Discard(q1)", 0.5); ([2.0; 1.0], "Discard(q1)", 0.5)];
      ];
    (* 9- Measure and parallel with if *)
    assertResult "M(q1 > x).c:quant!q1 || Tau.(if 0 = 0 then c1:int!1 else c1:int!2) \\ ()"
      [
        [([3.0; 2.0], "c:quant!q1 || c1:int!1", 0.5); ([2.0; 1.0], "c:quant!q1 || c1:int!1", 0.5)]
      ];
    (* 10- Measure and parallel with if *)
    assertResult "M(q1 > x).(if x = 0 then Discard(q1, q2) else Discard(q2, q1)) || Tau.c:int!1 \\ ()"
      [
        [([3.0; 2.0; 2.0; 2.0], "Discard(q2, q1) || c:int!1", 0.5);
         ([2.0; 1.0; 1.0; 1.0], "Discard(q1, q2) || c:int!1", 0.5)]
      ];
    (* 11- Triple parallel *)
    assertResult "Tau.Discard(q1) || Tau.Discard(q2) || Tau.Discard(q3) \\ ()"
      [
        [([1.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0; 0.0], "Discard(q3) || Discard(q1) || Discard(q2)", 1.0)]
      ];
    (* 12- Parallel with discard *)
    assertResult "Discard(q1) || Tau.Discard() \\ ()"
      [
        [([1.0; 0.0], "Discard(q1) || Discard()", 1.0)]
      ];
    (* 13- Parallel with send and receive *)
    assertResult "c:int!5 || c:int?x.Discard() \\ ()"
      [
        [([1.0], "Discard()", 1.0)]
      ];
    (* 14- Parallel with send, receive and expressions *)
    assertResult "c:int!(5+1) || c:int?x.if x = 6 then c:int!(x+1) else Discard() \\ ()"
      [
        [([1.0], "c:int!7", 1.0)]
      ];
    (* 15- QUANTUM LOTTERY *)
    assertResult "(H(q1).M(q1 > x).((if x = 0 then a:int!1 else b:int!1) || Discard(q1))) \\ ()"
      [
        [([3.0; 2.0], "Discard(q1) || b:int!1", 0.5);
         ([2.0; 1.0], "Discard(q1) || a:int!1", 0.5)]
      ];
    (* 16- TELEPORTATION (simplified) *)
    assertResult "(CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.c:int!n2) \\ (m:int)"
      [
        [([3.0; 2.0; 2.0; 2.0], "c:int!1 || Discard(q1, q2)", 0.5);
         ([2.0; 1.0; 1.0; 1.0], "c:int!0 || Discard(q1, q2)", 0.5)]
      ];
    (* 17- TELEPORTATION (full) *)
    assertResult "(CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.(if n2 = 0 then (I(q3).o:quant!q3) else (if n2 = 1 then (X(q3).o:quant!q3) else (if n2 = 2 then (Z(q3).o:quant!q3) else (Z(q3).X(q3).o:quant!q3))))) \\ (m:int)"
      [
        [([3.0; 2.0; 2.0; 2.0; 2.0; 2.0; 2.0; 2.0], "X(q3).o:quant!q3 || Discard(q1, q2)", 0.5);
         ([2.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0], "I(q3).o:quant!q3 || Discard(q1, q2)", 0.5)]
      ];
    (* 18- Send qbit and discard variable *)
    assertResult "c:quant!q1 || c:quant?x.Discard(x) \\ ()"
      [
        [([1.0; 0.0], "Discard(q1)", 1.0)]
      ];
  ]

let _ =
  Printf.printf "\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests