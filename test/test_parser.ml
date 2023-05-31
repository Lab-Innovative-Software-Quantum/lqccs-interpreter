open Lqccs

let assertException source ind =
  try 
    let lexbuf = Lexing.from_string ~with_positions:true source in
    Parsing.parse Scanner.next_token lexbuf |> ignore;
    Printf.printf "[FAIL] Test #%2d\n" ind
  with
  | Scanner.Lexing_error (_, msg)
  | Parsing.Syntax_error (_, msg) -> 
    Printf.printf "[ OK ] Test #%2d: %s\n" ind msg
  
let assertNotException source ind = 
  try 
    let lexbuf = Lexing.from_string ~with_positions:true source in
    Parsing.parse Scanner.next_token lexbuf |> ignore;
    Printf.printf "[ OK ] Test #%2d\n" ind
  with
  | Scanner.Lexing_error (_, msg)
  | Parsing.Syntax_error (_, msg) -> 
    Printf.printf "[FAIL] Test #%2d: %s\n" ind msg

let tests = [
  assertNotException "M(q1 > x).Discard() \\ ()";
  assertException "M(q1 > x).Discard(). \\ ()";
  assertNotException "c1:int?y.M(q1 > x).Discard(q1) \\ (c1: int)";
  assertNotException "H(q1).M(q1 > x).((if x=0 then a:int!1 else b:int!1) || Discard(q1)) \\ ()";
  assertException "c:int?q1.Discard(q1) \\ ()"; (* cannot receive on a variable that starts with q *)
  assertException "Discard() ++ (Discard() || Discard()) \\ ()"; (* cannot use par inside a choice *)
]

let _ = 
  Printf.printf "\n";
  List.iteri (fun ind funtest ->
    funtest (ind + 1)
  ) tests
