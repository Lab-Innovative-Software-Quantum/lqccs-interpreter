open Ast
open Location

exception TypeException of string * code_pos

let declare_var env (VarName(var)) loc vartype =
  try
    let v = Hashtbl.find env var in
    if v = vartype then () (* skip, if the variable was already defined *)
    else raise (TypeException((var ^ " already declared of type " ^ show_typ vartype), loc))
  with Not_found -> (* create variable with vtype in env *)
    Hashtbl.add env var vartype

let typecheck_access env access =
  match access with
  | { node = nod; loc } -> (
      match nod with
      | AccessVar (VarName(var)) -> 
        (* controllare che var esista e ritornare il suo tipo *)
        (try Hashtbl.find env var with Not_found ->
          raise (TypeException(("variable '" ^ var ^ "' not declared"), loc)))
      | AccessQBit v -> 
        (* ensure the qbit is higher than 1 or equal *)
        if v > 0 then TQuant
        else raise(TypeException("QBit index must be higher or equal to 1", loc)))

let rec typecheck_expr env expr =
  match expr with
  | { node = nod; loc } -> (
      match nod with
      | BinaryOp (binop, expr1, expr2) ->
          let typ1 = typecheck_expr env expr1 in
          let typ2 = typecheck_expr env expr2 in
          (* auxiliary function to raise meaningfull exception *)
          let fail_fun opname typ1 typ2 =
            raise(TypeException(opname ^ " " ^ (show_typ typ1) ^ " and " ^ (show_typ typ2), loc))
          in
          (* ensure all the operations are between the same types *)
          if typ1 <> typ2 then fail_fun "Invalid binary op between " typ1 typ2
          else(
            match binop with
            | Lt | Gt | Geq | Leq
            | Or | And -> (* ensure comparison between booleans *)
              if typ1 <> TBool then fail_fun "Cannot compare" typ1 typ2
              else TBool
            (* ensure quality between booleans or integers *)
            | Eq -> if typ1 = TQuant then fail_fun "Cannot compare" typ1 typ2 else typ1
            (* ensure sum between integers *)
            | Sum -> if typ1 <> TInt then fail_fun "Cannot sum" typ1 typ2 else TInt)
      | UnaryOp (uop, expr1) -> (
          let typ = typecheck_expr env expr1 in
          match uop with
          | Not -> if typ <> TBool then raise(TypeException("Cannot negate" ^ (show_typ typ), loc)) else TBool
          | Neg -> if typ <> TInt then raise(TypeException("Cannot negate" ^ (show_typ typ), loc)) else  TInt)
      | ILiteral _ -> TInt
      | BLiteral _ -> TBool
      | Access acc -> typecheck_access env acc)


let typecheck_restr env = function { node = Restr _; _ } -> env

let rec typecheck_internal_choice env internal_choice =
  match internal_choice with
  | { node; loc } -> (
      match node with
      | InternalChoice seq_list -> List.iter (typecheck_seq env) seq_list
      | IfThenElse (expr, inter_node_1, inter_node_2) ->
          if typecheck_expr env expr <> TBool then raise(TypeException("Only booleans are allowed as guar", loc)) else ();
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
  | { node = nod; loc } ->
    match nod with
    | Tau rest -> typecheck_internal_par env rest
    | Measure (qnames, var, rest) ->
        List.iter (fun qname -> 
          match typecheck_access env qname with
            | TQuant -> ()
            | anyTyp -> raise(TypeException("Cannot measure a " ^ show_typ anyTyp, loc))
        ) qnames;
        (* declare variable var of the type int 
           because the measurement returns an integer *)
        declare_var env var loc TInt;
        (* typecheck the rest of the program *)
        typecheck_internal_par env rest
    | QOp (qop, acclis, rest) -> 
      let given_types = List.map(typecheck_access env) acclis in
      let required_types = (match qop with
        | H | X | Y | Z -> [TQuant]
        | CX -> [TBool; TQuant]) in
      (* ensure the given types are the SAME of the required *)
      let are_equal = List.equal (=) given_types required_types in
      if not are_equal then 
        raise(TypeException("Quantum operation used with invalid types or invalid number of arguments", loc))
      (* typecheck the rest of the program *)
      else typecheck_internal_par env rest
    | Recv (Chan(_, chantyp), var, rest) -> 
      (* declare variable var of the same type of the receiving channel *)
      declare_var env var loc chantyp;
      (* typecheck the rest of the program *)
      typecheck_internal_par env rest
    | Send (Chan(_, chantyp), exp) ->
      (* typecheck exp and check that its type is the channel's type *)
      let exptyp = typecheck_expr env exp in
      if chantyp <> exptyp
      then raise(TypeException("Cannot send a " ^ show_typ exptyp ^ " through a " ^ show_typ chantyp ^ " channel", loc))
    | Discard qnames ->
        List.iter (fun qname -> 
          match typecheck_access env qname with
            | TQuant -> ()
            | anyTyp -> raise(TypeException("Cannot discard a " ^ show_typ anyTyp, loc))
        ) qnames

let typecheck_external_choice env external_choice =
  match external_choice with
  | { node = ExternalChoice(seq_list); _ } -> 
      List.iter (typecheck_seq env) seq_list

let typecheck_external_par env external_par =
  match external_par with
  | { node = ExternalPar external_choice_list; _ } ->
      List.iter (typecheck_external_choice env) external_choice_list

let typecheck_program env prog =
  match prog with
  | Prog(external_par, restr) ->
      typecheck_external_par env external_par;
      typecheck_restr env restr

let typecheck (ast : program) =
  let env = Hashtbl.create 256 in
  typecheck_program env ast |> ignore;
  ast
