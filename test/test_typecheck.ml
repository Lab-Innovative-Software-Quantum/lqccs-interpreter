open Lqccsint.Typechecker
open Lqccsint.Ast
open Lqccsint.Location

let node n = { node = n; loc = dummy_code_pos }

let assertException test ind =
  try 
    typecheck test |> ignore
  with TypeException(msg, _) -> 
    Printf.printf "[ OK ] Test #%2d: %s\n" ind msg
  
let assertNotException test ind = 
  try 
    typecheck test |> ignore;
    Printf.printf "[ OK ] Test #%2d\n" ind
  with TypeException(msg, _) -> 
    Printf.printf "[FAIL] Test #%2d: %s\n" ind msg
     
let tests = [
  assertException(Prog(
    node(ExternalPar(
      [node(ExternalChoice(
        [node(Discard([
            node(AccessVar(
              VarName "undeclared"
            ))
          ]))
        ])
      )]
    )),
    node(Restr([]))
  ));
  assertException(Prog(
    node(ExternalPar(
      [node(ExternalChoice(
        [node(
          Measure([
            node(AccessQBit(1))
          ],
          VarName("x"),
          node(
            InternalPar([
              node(
                InternalChoice([
                  node(Discard([
                    node(AccessQBit(
                      1
                    ))
                  ]))
                ])
              )
            ])
          )
          ))
        ])
      )]
    )),
    node(Restr([]))
  ));
]

let _ = 
  Printf.printf "\n";
  List.iteri (fun ind funtest ->
    funtest (ind + 1)
  ) tests
