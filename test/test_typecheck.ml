open Lqccs

let assertException source ind =
  try
    let lexbuf = Lexing.from_string ~with_positions:true source in
    let ast = Parsing.parse Scanner.next_token lexbuf in
    (* Printf.printf "%s" (Ast.show_program ast); *)
    Typechecker.typecheck ast |> ignore;
    Printf.printf "\027[1;31m[FAIL] Test #%2d should fail but doesn't\n\027[0m"
      ind
  with
  | Scanner.Lexing_error (pos, msg) | Parsing.Syntax_error (pos, msg) ->
      let header = Printf.sprintf "[FAIL] Test #%2d" ind in
      Lqccs.Errors.report_singleline header source pos msg;
      Stdlib.flush_all ()
  | Typechecker.TypeException (msg, _) ->
      Printf.printf "[ OK ] Test #%2d: %s\n" ind msg

let assertNotException source ind =
  try
    let lexbuf = Lexing.from_string ~with_positions:true source in
    let ast = Parsing.parse Scanner.next_token lexbuf in
    (* Printf.printf "%s" (Ast.show_program ast); *)
    Typechecker.typecheck ast |> ignore;
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

let tests =
  [
    (* 1- Discard an undeclared variable *)
    assertException "Discard(undeclared) \\ ()";
    (* 2- Empty discard *)
    assertNotException "Discard() \\ ()";
    (* 3- Discard q1 *)
    assertNotException "Discard(q1) \\ ()";
    (* 4- Measure q1 and discard it *)
    assertNotException "M(q1 > x).Discard(q1) \\ ()";
    (* 5- Same qbit used in parallel proccesses *)
    assertException "Discard(q1) || Discard(q1) \\ ()";
    (* 6- Quantum lottery *)
    assertNotException
      "H(q1).M(q1 > x).((if (x=0) then a:int!1 else b:int!1) || Discard(q1)) \
       \\ ()";
    (* 7- If then else with tau and send *)
    assertException
      "Tau.(if (y=0) then a:int!1 else b:int!1) || Discard(q1) \\ ()";
    (* 8- Try to use an int as quantum variable *)
    assertException "c:int?x.H(x).Discard() \\ ()";
    (* 9- Try to discard an int *)
    assertException "c:int?x.Discard(x) \\ ()";
    (* 10- Receive a quantum variable and use it *)
    assertNotException "c:quant?x.H(x).Discard(x) \\ ()";
    (* 11- Try to use same chan with different type *)
    assertException "c:int?x.c:bool?y.Discard() \\ ()";
    (* 12- Receive same channels with same type *)
    assertNotException "c:int?x.c:int?y.Discard() \\ ()";
    (* 13- Try to measure an int *)
    assertException "c:int?x.M(x > y).Discard() \\ ()";
    (* 14- Try to put measurement in bool variable *)
    assertException "c:bool?x.M(q1 > x).Discard(q1) \\ ()";
    (* 15- Try to send measurement on bool chan *)
    assertException "M(q1 > x).(c:bool!x || Discard(q1)) \\ ()";
    (* 16- Discard multiple qbits *)
    assertNotException "H(q1).H(q2).Discard(q1,q2) \\ ()";
    (* 17- Try to use CX with only one qbit *)
    assertException "CX(q1).Discard(q1) \\ ()";
    (* 18- Use q1 without discarding *)
    assertException "H(q1).Discard() \\ ()";
    (* 19- Measure and send measurement without discarding *)
    assertException "M(q1 > x).c:int!x \\ ()";
    (* 20- Try to send bool on int chan *)
    assertException "c:int!true \\ ()";
    (* 21- Try to send int on bool chan *)
    assertException "c:bool!10 \\ ()";
    (* 22- Parallel process with shared channel *)
    assertException "c:int!10 || c:bool?x.Discard() \\ ()";
    (* 23- Choice with simple expr *)
    assertNotException "c:int!10 ++ c:int!20 \\ ()";
    (* 24- Choice with sum *)
    assertNotException "c:int!10+1 ++ c:int!20 \\ ()";
    (* 25- The intersection of all Sigmas does not contain AccessQBits (but it may contain AccessVars) *)
    assertNotException
      "c1:quant?x.Discard(q1,x) || c2:quant?x1.Discard(q2,x1) \\ ()";
    (* 26- Channel type different from restriction *)
    assertException "c:bool?x.Discard() \\ (c:int)";
    (* 27- IfThenElse must present the same Sigma for "then" and "else" branches *)
    assertException "Tau.(if 1=1 then Discard(q1) else Discard(q2)) \\ ()";
    (* 28- Channel type same as restriction *)
    assertNotException "c:int?x.Discard() \\ (c:int)";
    (* 29- Try not to discard received quantum variable *)
    assertException "c1:quant?x.M(x > y).c2:int!y \\ ()";
    (* 30- Sigma of both processes in P++Q does not check also received values *)
    assertNotException "Discard(q1) ++ c:quant?x.Discard(x,q1) \\ ()";
    (* 31- Sigma of both processes is not the same *)
    assertException "Discard(q1) ++ c:quant?x.Discard(x) \\ ()";
    (* 32- Sigma of all 3 processes is the same *)
    assertNotException 
      "c:quant?x.(c2:quant!x ++ Discard(x) ++ H(x).Discard(x)) \\ ()";
    (* 33- Sigma of all 3 processes is the same *)
    assertNotException
      "(c:quant?x.c2:quant!x ++ c:quant?x1.Discard(x1) ++ \
       c:quant?x2.H(x2).Discard(x2)) \\ ()";
    (* 34- Try not to discard x *)
    assertException "Discard(q1) ++ c:quant?x.H(x).Discard(q1) \\ ()";
    (* 35- Intersection is empty *)
    assertNotException "Discard(q1) || c:quant?x.(Discard(x)) \\ ()";
    (* 36- Variable x is only a name, local to the processes *)
    assertNotException "c1:quant?x.Discard(x) || c2:quant?x1.Discard(x1) \\ ()";
    (* 37- Choices share q1 but parallel don't *)
    assertNotException
      "Discard(q1) ++ Tau.(Discard(q1) || c:quant?x.(Discard(x))) \\ ()";
    (* 38- qvar are linear like qnames *)
    assertException
      "c1:quant?x.(H(x).Discard(q1,x) || X(x).Discard(q2,x)) \\ ()";
    (* 39 *)
    assertNotException
      "c1:quant?x.(c2:quant?x1.H(x1).Discard(q1,x1) || X(x).Discard(q2,x)) \\ ()";
    (* 40- Distinctness of names in CX *)
    assertException "CX(q1,q1).Discard(q1) \\ ()";
    (* 41- Check number of arguments of single qbit operations *)
    assertException "H(q1,q1).Discard(q1) \\ ()";
    (* 42- Not of int *)
    assertException "c1:int?x.c2:bool!not x \\ ()";
    (* 43- Not of bool *)
    assertNotException "c:bool?x.c:bool!not x \\ ()";
  ]

let _ =
  Printf.printf "\n--- Typechecker ---\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests