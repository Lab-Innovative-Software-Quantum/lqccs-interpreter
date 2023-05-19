(*
1. build an environment to map variable name to its type
2. for each send, ensure variable's type is the same as the channel's type
3. for each recv, add a new variable to the environment and give the same type as the channel
4. ensure the expressions are valid (e.g comparison between ints, not bools)
*)
open Ast
open Location

exception TypeException of string

let node arg = { loc = Location.dummy_code_pos; node = arg }

let type_to_string t =
  match t with TInt -> "TInt" | TBool -> "TBool" | TQuant -> "TQuant"

let rec typecheck_expr env expr =
  match expr with
  | { node = nod; _ } -> (
      match nod with
      | BinaryOp (binop, expr1, expr2) -> (
          let typ1 = typecheck_expr env expr1 in
          let typ2 = typecheck_expr env expr2 in
          if typ1 <> typ2 then failwith ""
          else
            match binop with
            | Lt -> if typ1 <> TInt then failwith "" else TBool
            | Gt -> if typ1 <> TInt then failwith "" else TBool
            | Geq -> if typ1 <> TInt then failwith "" else TBool
            | Leq -> if typ1 <> TInt then failwith "" else TBool
            | Eq -> if typ1 = TQuant then failwith "" else typ1
            | Or -> if typ1 <> TBool then failwith "" else TBool
            | And -> if typ1 <> TBool then failwith "" else TBool
            | Sum -> if typ1 <> TInt then failwith "" else TInt)
      | UnaryOp (uop, expr1) -> (
          let typ = typecheck_expr env expr1 in
          match uop with
          | Not -> if typ <> TBool then failwith "" else TBool
          | Neg -> if typ <> TInt then failwith "" else TInt)
      | ILiteral n -> TInt
      | BLiteral b -> TBool
      | Access acc -> () (* TODO: *))

let rec typecheck_var env var vtype =
  try
    let v = Hashtbl.find env var in
    if v = vtype then ()
    else raise (TypeException (var ^ " is not of type " ^ type_to_string vtype))
  with Not_found ->
    (* create variable with vtype in env *)
    Hashtbl.add env var vtype

let typecheck_access env access =
  match access with
  | { node = nod; _ } -> (
      match nod with
      | AccessVar var -> typecheck_var env var (*TODO: *)
      | AccessQBit v -> ())

let typecheck_restr env = function { node = Restr channels; _ } -> env

let rec typecheck_internal_choice env internal_choice =
  match internal_choice with
  | { node; _ } -> (
      match node with
      | InternalChoice seq_list -> List.iter (typecheck_seq env) seq_list
      | IfThenElse (expr, inter_node_1, inter_node_2) ->
          if typecheck_expr env expr <> TBool then failwith "" else ();
          typecheck_internal_par env inter_node_1;
          typecheck_internal_par env inter_node_2)

and typecheck_internal_par env internal_par =
  match internal_par with
  | { node; _ } -> (
      match node with
      | InternalPar internal_choice_list ->
          List.iter (typecheck_internal_choice env) internal_choice_list)

and typecheck_seq env seq =
  match seq with
  | { node = nod; _ } -> (
      try
        match nod with
        | Tau rest -> typecheck_internal_par env rest
        | Measure (qnames, var, rest) ->
            List.iter (typecheck_access env) qnames;
            typecheck_var env var TInt;

            typecheck_internal_par env rest
        | QOp (qop, qnames, rest) -> ()
        | Recv (chan, var, rest) -> ()
        | Send (chan, exp) -> ()
        | Discard qnames -> ()
      with TypeException msg ->
        raise (TypeException ("Error at line " ^ " location" ^ ": " ^ msg)))

let typecheck_external_choice env external_choice =
  match external_choice with
  | { node; _ } -> (
      match node with
      | ExternalChoice seq_list -> List.iter (typecheck_seq env) seq_list)

let typecheck_external_par env external_par =
  match external_par with
  | { node; _ } -> (
      match node with
      | ExternalPar external_choice_list ->
          List.iter (typecheck_external_choice env) external_choice_list)

let typecheck_program env prog =
  match prog with
  | Prog (external_par, restr) ->
      typecheck_external_par env external_par;
      typecheck_restr env restr

let typecheck (ast : program) =
  let env = Hashtbl.create 256 in
  typecheck_program env ast
