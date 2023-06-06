open Lqccs

let assertException source ind =
  try
    let lexbuf = Lexing.from_string ~with_positions:true source in
    let ast = Parsing.parse Scanner.next_token lexbuf in
    (* Printf.printf "%s" (Ast.show_program ast); *)
    Typechecker.typecheck ast |> Eval.eval |> ignore;
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
    Typechecker.typecheck ast |> Eval.eval |> ignore;
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
  ]

let _ =
  Printf.printf "\n";
  List.iteri (fun ind funtest -> funtest (ind + 1)) tests