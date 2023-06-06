open Ast
open Symbol_table
(* open Typechecker *)

exception EvalException of Location.code_pos * string

(*type ast_element =
  | Seq of seq
  | ExtChoice of external_choice
  | IntChoice of internal_choice
  | ExtPar of external_par
  | IntPar of internal_par

(* qbits and the probability, which is expressed in the range [0, 1] *)
type qstate = float list
type conf = Conf of qstate * ast_element * float

(*
[Conf([1/sqrt(2);0], a!1, 0.5); [1;1], b!1, Conf(0.5)]
*)
(* ((c1!1 || c1?x) ++ (c2!1 || c2?x)).print x *)

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
      | Access acc -> eval_access symtbl acc)

and eval_internal_par symtbl internal_par =
  match internal_par with
  | { node = InternalPar internal_choice_list; _ } -> []

and eval_seq symtbl seq (Conf(qst, _, prob)) =
  match seq with
  | { node = nod; loc } ->
      match nod with
      | Tau rest -> Some(Conf(qst, IntPar(rest), prob))
      | Measure (_, VarName(vname), rest) ->
          (try 
            let symtbl = add_entry (VarName(vname)) (QBit(0)) symtbl in
            Some(Conf(qst, IntPar(rest), prob))
          with Not_found -> raise(TypeException("Variable " ^ vname ^ " already declared", loc)))
      | QOp (_, _, rest) ->
          Some(Conf(qst, IntPar(rest), prob))
      | Recv (Chan (_, _), _, rest) ->
          Some(Conf(qst, IntPar(rest), prob))
      | Send (Chan (_, _), _) -> 
          None (* return None such that the caller knows this process is ended *)
      | Discard _ -> 
          None (* return None such that the caller knows this process is ended *)
*)
(*let eval_external_par symtbl external_par =
  match external_par with
  | { node = ExternalPar external_choice_list; _ } -> []*)

let eval_conf _ = () (*match cnf with
  | (Conf(_, Seq(seq), _)) -> 
      (* return the configuration after the first seq is executed *)
      eval_seq symtbl seq cnf
  | (Conf(qs, ExtChoice({ node = ExternalChoice(choice_lis); _ }), prob))
  | (Conf(qs, IntChoice({ node = InternalChoice(choice_lis); _ }), prob)) -> 
      eval_choice symtbl qs prob choice_lis
  | (Conf(qs, IntChoice({ node = IfThenElse(expr, then_branch, else_branch); loc }), prob)) -> 
      (match eval_expr symtbl expr with
        | Bool(true)  -> eval_conf symtbl (Conf(qs, IntPar(then_branch), prob))
        | Bool(false) -> eval_conf symtbl (Conf(qs, IntPar(else_branch), prob))
        | _ -> raise (TypeException ("Invalid guard", loc)))
  | (Conf(_, ExtPar({ node = ExternalPar(ext_choice_lis); _ }), _)) -> 
      let (tobe_exec, rest_of_lis) = get_next ext_choice_lis in
      let new_conf = eval_conf symtbl tobe_exec in
      eval_conf symtbl new_conf
  | (Conf(_, IntPar(internal_par), _)) -> 
      let new_conf = eval_internal_par symtbl internal_par in
      eval_conf symtbl new_conf *)

(*and eval_choice symtbl qs prob choices =
  List.fold_left (fun acc choice ->
    let new_scope = begin_block symtbl in
    let new_conf_opt = eval_conf new_scope (Conf(qs, Seq(choice), prob)) in
    match new_conf_opt with
      | Some new_conf -> new_conf::acc
      | None -> acc
  ) [] choices*)

let eval (_ : program) =
  (* build empty symbol table *)
  let symtbl = begin_block empty_table in
  (* build the starting configuration. It is an external par *)
  (*let starting_conf = match prog with
    | Prog (ext_par, _) -> 
      (Conf([], ExtPar(ext_par), 1.0)) in *)
  (* finally start by evaluating the starting configuration *)
  eval_conf symtbl |> ignore

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
