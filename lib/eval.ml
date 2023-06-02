open Ast
open Symbol_table
open Typechecker

exception EvalException of Location.code_pos * string

(* qbits and the probability, which is expressed in the range [0, 1] 
type 'a conf = Result of int list * 'a * float*)

type value = Int of int | Bool of bool | QBit of int

let eval_binop (bop : binop) (v1 : value) (v2 : value) (loc: Location.code_pos): value =
  match (bop, v1, v2) with
  | Sum, Int a, Int b   -> Int (a + b)
  | Eq, Int a, Int b    -> Bool (a == b)
  | Eq, Bool a, Bool b  -> Bool (a == b)
  | Gt, Int a, Int b    -> Bool (a > b)
  | Geq, Int a, Int b   -> Bool (a >= b)
  | Lt, Int a, Int b    -> Bool (a < b)
  | Leq, Int a, Int b   -> Bool (a <= b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b  -> Bool (a || b)
  | _ -> raise (TypeException ("Type error", loc))

let eval_uop (op : uop) (v : value) (loc: Location.code_pos) : value = 
  match (op, v) with
  | Not, Bool lit -> Bool (not lit)
  | Neg, Int lit  -> Int (-lit)
  | _ -> raise (TypeException ("Type error", loc))

let eval_access symtbl access =
  match access with
  | { node = nod; loc } ->
      (match nod with
      | AccessVar (var) ->
        (match lookup var symtbl with
          | Some content -> content
          | None -> raise (TypeException ("Variable not declared", loc)))
      | AccessQBit qb -> QBit(qb))

let rec eval_expr symtbl expr =
  match expr with
  | { node = nod; loc } -> (
      match nod with
      | BinaryOp (op, e1, e2) -> 
        let v1 = eval_expr symtbl e1 in
        let v2 = eval_expr symtbl e2 in
        eval_binop op v1 v2 loc
      | UnaryOp (op, e1) -> 
        let v1 = eval_expr symtbl e1 in
        eval_uop op v1 loc
      | ILiteral lit -> Int(lit)
      | BLiteral lit -> Bool(lit)
      | Access acc -> eval_access symtbl acc |> ignore; Int(17))

let rec eval_choice symtbl seq_list _ = 
  List.map (fun choice ->
    let new_scope = begin_block symtbl in
    eval_seq new_scope choice
  ) seq_list

and eval_internal_choice symtbl internal_choice =
  match internal_choice with
  | { node; loc } -> (
      match node with
      | InternalChoice seq_list -> eval_choice symtbl seq_list loc
      | IfThenElse (expr, then_branch, else_branch) -> 
          match eval_expr symtbl expr with
            | Bool(true) -> eval_internal_par symtbl then_branch
            | Bool(false) -> eval_internal_par symtbl else_branch
            | _ -> raise (TypeException ("Invalid guard", loc)))

and eval_internal_par symtbl internal_par =
  match internal_par with
  | { node = InternalPar internal_choice_list; _ } ->
      List.map (eval_internal_choice symtbl) internal_choice_list |> ignore; []

and eval_seq symtbl seq =
  match seq with
  | { node = nod; _ } -> (
      match nod with
      | Tau rest -> eval_internal_par symtbl rest |> ignore; []
      | Measure (_, _, rest) ->
          (* eval the rest of the program *)
          eval_internal_par symtbl rest |> ignore; []
      | QOp (_, _, rest) ->
          (* eval the rest of the program *)
          eval_internal_par symtbl rest |> ignore; []
      | Recv (Chan (_, _), _, rest) ->
          (* eval the rest of the program *)
          eval_internal_par symtbl rest |> ignore; []
      | Send (Chan (_, _), _) -> [()]
      | Discard _ -> [()])

let eval_external_choice symtbl external_choice =
  match external_choice with
  | { node = ExternalChoice seq_list; loc } -> eval_choice symtbl seq_list loc

let eval_external_par symtbl external_par =
  match external_par with
  | { node = ExternalPar external_choice_list; _ } ->
      let _ = List.map (eval_external_choice symtbl) external_choice_list in
      ()

let eval_program symtbl prog =
  match prog with
  | Prog (external_par, _) -> eval_external_par symtbl external_par

let eval (prog : program) =
  let symtbl = begin_block empty_table in
  eval_program symtbl prog

(*
   Configuration = [ quantum state, ast, probability ]

   QL
   [
     [ [0, H(q).ast, 1] ],
     [ [+, M(q>x).ast, 1] ],
     [ [(0; x=0), ifthenelse || d, 0.5], [(1;x=1), ifthenelse || d, 0.5] ],
     [ [(0; x=0), a!1 || d, 0.5], [(1;x=1), b!1 || d, 0.5] ],
   ]

   eval list = match list with
     | [] -> []
     | x::xs -> (eval x) @ (eval xs)

   [0, H.ast, 1] :: []
   [+, M.ast, 1] :: []
   [0, if.ast, 0.5] :: [1, if.ast, 0.5] :: []

   [(m(q>x) ++ d1 ++ d2, 1)]
   [ [(m(q>x),1)]; [(d1,1)]; [(d2,1)] ]
   [ [(+,0.5), (-,0.5)]; [(d1,1)]; [(d2,1)] ]

   [+,0.5; -,0.5]; [d1,0]; [d2,0]
*)
