open Lqccs
open Lqccs.String_of
open Lqccs.Eval
open Complex

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
    Printf.printf "[ OK ] Test #%2d\n" ind;
    Stdlib.flush_all ()
  with
  | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
      let header = Printf.sprintf "\027[1;31m[FAIL] Test #%2d\027[0m" ind in
      Lqccs.Errors.report_singleline header source pos msg;
      Stdlib.flush_all ()
  | Typechecker.TypeException (msg, pos) ->
      let header = Printf.sprintf "\027[1;31m[FAIL] Test #%2d\027[0m" ind in
      Lqccs.Errors.report_multiline header source pos msg;
      Stdlib.flush_all ()
  | Eval.EvalException (pos, msg) ->
      let header = Printf.sprintf "\027[1;31m[FAIL] Test #%2d\027[0m" ind in
      Errors.report_multiline header source pos msg;
      Stdlib.flush_all ()
  | WrongQstate given_conf ->
      Printf.printf "\027[1;31m[FAIL] Test #%2d: bad quantum state in conf %s\n\027[0m" ind
        (string_of_conf given_conf)
  | WrongAst given_conf ->
      Printf.printf "\027[1;31m[FAIL] Test #%2d: bad ast in conf %s\n\027[0m" ind
        (string_of_conf given_conf)
  | WrongProb given_conf ->
      Printf.printf "\027[1;31m[FAIL] Test #%2d: bad probability in conf %s\n\027[0m" ind
        (string_of_conf given_conf)
  | Invalid_argument _ ->
      Printf.printf "\027[1;31m[FAIL] Test #%2d: result lists have different length\n\027[0m" ind
  | exn ->
      Printf.printf "\027[1;31m[FAIL] Test #%2d: %s\n" ind (Printexc.to_string exn)

let minus_one = {re = -1.0; im = 0.0}

let tests =
  [
    (* 1- Discard a variable *)
    assertResult "M(q1 > x).Discard(q1) \\ ()"
      [
        [([one; zero], "Discard(q1)", 1.0)];
      ];
    (* 2- Empty discard after a tau *)
    assertResult "Tau.Discard() \\ ()" [ [ ([one], "Discard()", 1.0) ] ];
    (* 3- Parallel after measurement *)
    assertResult "M(q1 > x).Discard(q1) || Discard() \\ ()"
      [
        [([one; zero], "Discard() || Discard(q1)", 1.0)];
      ];
    (* 4- Choice after measurement *)
    assertResult "(M(q1 > x).(Discard(q1) ++ c:quant!q1)) \\ ()"
      [
        [([one; zero], "Discard(q1) ++ c:quant!q1", 1.0)];
      ];
    (* 5- Choice with send *)
    assertResult "(Discard(q1) ++ c:quant!q1) \\ ()"
      [
        [([one; zero], "Discard(q1) ++ c:quant!q1", 1.0) ];
      ];
    (* 6- If after tau *)
    assertResult "Tau.if 0 = 0 then Discard() else c:int!5 \\ ()" [[([one], "Discard()", 1.0)]];
    (* 7- Parallel of choices *)
    assertResult "Tau.Discard(q2) ++ Tau.Discard(q2) || Tau.Discard(q1) ++ Tau.Discard(q1) \\ ()"
      [
        [([one; zero; zero; zero], "Discard(q2) || Discard(q1)", 1.0)];
        [([one; zero; zero; zero], "Discard(q2) || Discard(q1)", 1.0)];
        [([one; zero; zero; zero], "Discard(q2) || Discard(q1)", 1.0)];
        [([one; zero; zero; zero], "Discard(q2) || Discard(q1)", 1.0)];
      ];
    (* 8- Measure and choice with if *)
    assertResult "M(q1 > x).(Discard(q1) ++ Tau.if x = 0 then Tau.Discard(q1) else Discard(q1)) \\ ()"
      [
        [([one; zero], "Discard(q1)", 1.0)];
      ];
    (* 9- Measure and parallel with if *)
    assertResult "M(q1 > x).c:quant!q1 || Tau.(if 0 = 0 then c1:int!1 else c1:int!2) \\ ()"
      [
        [([one; zero], "c:quant!q1 || c1:int!1", 1.0)]
      ];
    (* 10- Measure and parallel with if *)
    assertResult "M(q1 > x).(if x = 0 then Discard(q1, q2) else Discard(q2, q1)) || Tau.c:int!1 \\ ()"
      [
        [([one; zero; zero; zero], "Discard(q1, q2) || c:int!1", 1.0)]
      ];
    (* 11- Triple parallel *)
    assertResult "Tau.Discard(q1) || Tau.Discard(q2) || Tau.Discard(q3) \\ ()"
      [
        [([one; zero; zero; zero; zero; zero; zero; zero], "Discard(q1) || Discard(q2) || Discard(q3)", 1.0)]
      ];
    (* 12- Parallel with discard *)
    assertResult "Discard(q1) || Tau.Discard() \\ ()"
      [
        [([one; zero], "Discard(q1) || Discard()", 1.0)]
      ];
    (* 13- Parallel with send and receive *)
    assertResult "c:int!5 || c:int?x.Discard() \\ ()"
      [
        [([one], "Discard()", 1.0)]
      ];
    (* 14- Parallel with send, receive and expressions *)
    assertResult "c:int!(5+1) || c:int?x.if x = 6 then c:int!(x+1) else Discard() \\ ()"
      [
        [([one], "c:int!7", 1.0)]
      ];
    (* 15- QUANTUM LOTTERY *)
    assertResult "(H(q1).M(q1 > x).((if x = 0 then a:int!1 else b:int!1) || Discard(q1))) \\ ()"
      [
        [
          ([zero; one], "Discard(q1) || b:int!1", 0.5);
          ([one; zero], "Discard(q1) || a:int!1", 0.5);
        ]
      ];
    (* 16- TELEPORTATION (simplified) *)
    assertResult "(CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.c:int!n2) \\ (m:int)"
      [
        [([zero; zero; one; zero], "Discard(q1, q2) || c:int!1", 0.5);
         ([one; zero; zero; zero], "Discard(q1, q2) || c:int!0", 0.5)]
      ];
    (* 17- TELEPORTATION (in the paper) *)
    assertResult "(CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.(if n2 = 0 then (I(q3).o:quant!q3) else (if n2 = 1 then (X(q3).o:quant!q3) else (if n2 = 2 then (Z(q3).o:quant!q3) else (Z(q3).X(q3).o:quant!q3))))) \\ (m:int)"
      [
        [([zero; zero; zero; zero; zero; one; zero; zero], "Discard(q1, q2) || o:quant!q3", 0.5);
         ([one; zero; zero; zero; zero; zero; zero; zero], "Discard(q1, q2) || o:quant!q3", 0.5)]
      ];
    (* 18- TELEPORTATION (with entanglement) *)
    assertResult "H(q2).CX(q2,q3).((CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.(if n2 = 0 then (I(q3).o:quant!q3) else (if n2 = 1 then (X(q3).o:quant!q3) else (if n2 = 2 then (Z(q3).o:quant!q3) else (Z(q3).X(q3).o:quant!q3)))))) \\ (m:int)"
      [
        [([zero; zero; zero; zero; zero; zero; minus_one; zero], "Discard(q1, q2) || o:quant!q3", 0.25);
         ([zero; zero; zero; zero; zero; one; zero; zero], "Discard(q1, q2) || o:quant!q3", 0.25);
         ([zero; zero; zero; minus_one; zero; zero; zero; zero], "Discard(q1, q2) || o:quant!q3", 0.25);
         ([one; zero; zero; zero; zero; zero; zero; zero], "Discard(q1, q2) || o:quant!q3", 0.25)]
      ];
    (* 19- Send qbit and discard variable *)
    assertResult "c:quant!q1 || c:quant?x.Discard(x) \\ ()"
      [
        [([one; zero], "Discard(q1)", 1.0)]
      ];
    (* 20- error 2 from professors feedback *)
    assertResult "(c:quant!q1 ++ o:quant!q1) || c:quant?x.Discard(x) \\ ()"
      [
        [([one; zero], "Discard(q1)", 1.0)]
      ];
    (* 21- error 4 from professors feedback *)
    assertResult "d:int?x.e:int!0 || c:int?z.(d:int!1 || e:int?y.if z = 0 then a:int!42 else b:int!666) || c:int!0 \\ ()"
      [
        [([one]), "a:int!42", 1.0]
      ];
    (* 22- error 1 from professors feedback *)
    assertResult "a:int!0 ++ Discard() || Tau.b:int!1 \\ ()"
      [
        [([one], "a:int!0 ++ Discard() || b:int!1", 1.0)];
      ];
    (* 23- error from professors feedback *)
    assertResult "(a:int!1 ++ a:int!2) || a:int?x.b:int!x \\ ()"
      [
        [([one], "b:int!1", 1.0)];
        [([one], "b:int!2", 1.0)]
      ];
    (* 24- error from professors feedback *)
    assertResult "a:int!1 || a:int?x.b:int!x ++ a:int?y.c:int!y \\ ()"
      [
        [([one], "b:int!1", 1.0)];
        [([one], "c:int!1", 1.0)]
      ];
  ]

  let _ =
  Printf.printf "\n--- Eval ---\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests
