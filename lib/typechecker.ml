open Ast
open Location

exception TypeException of string * code_pos

module SigmaElement = struct
  type t = int * bool
  let compare (qbit1, _) (qbit2, _) = compare qbit1 qbit2
end

module SigmaSet = Set.Make(SigmaElement)

let declare_var gamma (VarName(var)) loc vartype =
  try
    let v = Hashtbl.find gamma var in
    if v = vartype then () (* skip, if the variable was already defined *)
    else raise (TypeException((var ^ " already declared of type " ^ show_typ vartype), loc))
  with Not_found -> (* create variable with vtype in gamma *)
    Hashtbl.add gamma var vartype

let typecheck_access gamma access =
  match access with
  | { node = nod; loc } -> (
      match nod with
      | AccessVar (VarName(var)) -> 
        (* controllare che var esista e ritornare il suo tipo *)
        (try (Hashtbl.find gamma var, None) with Not_found ->
          raise (TypeException(("Variable '" ^ var ^ "' not declared"), loc)))
      | AccessQBit v ->
        (* ensure the qbit is higher than 1 or equal *)
        if v > 0 then TQuant, Option.some v
        else raise(TypeException("QBit index must be higher or equal to 1", loc)))

let rec typecheck_expr gamma expr =
  match expr with
  | { node = nod; loc } ->
      match nod with
      | BinaryOp (binop, expr1, expr2) ->
          let typ1, _ = typecheck_expr gamma expr1 in
          let typ2, _ = typecheck_expr gamma expr2 in
          (* auxiliary function to raise meaningfull exception *)
          let fail_fun opname typ1 typ2 =
            raise(TypeException(opname ^ " " ^ (show_typ typ1) ^ " and " ^ (show_typ typ2), loc))
          in
          (* ensure all the operations are between the same types *)
          if typ1 <> typ2 then fail_fun "Invalid binary op between " typ1 typ2
          else
            let binoptyp = (match binop with
            | Lt | Gt | Geq | Leq
            | Or | And -> (* ensure comparison between booleans *)
              if typ1 <> TBool then fail_fun "Cannot compare" typ1 typ2
              else TBool
            (* ensure quality between booleans or integers *)
            | Eq -> if typ1 = TQuant then fail_fun "Cannot compare" typ1 typ2 else typ1
            (* ensure sum between integers *)
            | Sum -> if typ1 <> TInt then fail_fun "Cannot sum" typ1 typ2 else TInt) in
            binoptyp, Option.None
      | UnaryOp (uop, expr1) ->
          let typ, _ = typecheck_expr gamma expr1 in
          let uoptyp = (match uop with
          | Not -> if typ <> TBool then raise(TypeException("Cannot negate" ^ (show_typ typ), loc)) else TBool
          | Neg -> if typ <> TInt then raise(TypeException("Cannot negate" ^ (show_typ typ), loc)) else  TInt) in
          uoptyp, Option.None
      | ILiteral _ -> TInt, Option.None
      | BLiteral _ -> TBool, Option.None
      | Access acc -> typecheck_access gamma acc

let typecheck_restr gamma = function { node = Restr _; _ } -> gamma

let rec typecheck_choice gamma seq_list loc =
  match seq_list with
    [] -> raise(TypeException("Non-deterministic choice needs at least 2 or more processes", loc))
  | head::rest -> 
    (* process first seq *)
    let sigma_head = typecheck_seq gamma head in
    (* process all the other seq *)
    let sigma_rest = List.map (typecheck_seq gamma) rest in
    (* for each sigma from the rest of seqs, check they are equal to the first one.
        in practice we ensure that all the sigmas are equal *)
    List.iter (fun sigma_i ->
      if not(SigmaSet.equal sigma_head sigma_i) then raise(TypeException("Branches of a non-deterministic choice must share quantum variables", loc))
    ) sigma_rest;
    sigma_head

and check_par_intersection loc = List.fold_left (fun acc set -> 
  if (SigmaSet.inter set acc) == SigmaSet.empty
  then SigmaSet.union set acc
  else raise(TypeException("Parallel processes cannot share quantum variables", loc))
) SigmaSet.empty

and typecheck_internal_choice gamma internal_choice =
  match internal_choice with
  | { node; loc } ->
    match node with
    | InternalChoice seq_list ->
      typecheck_choice gamma seq_list loc
    | IfThenElse (expr, inter_node_1, inter_node_2) ->
      let guardtyp, _ = typecheck_expr gamma expr in
      if guardtyp <> TBool then raise(TypeException("Only booleans are allowed as guard", loc));
      let sigma_then = typecheck_internal_par gamma inter_node_1 in
      let sigma_else = typecheck_internal_par gamma inter_node_2 in
      if not(SigmaSet.equal sigma_then sigma_else) then raise(TypeException("Branches of a conditional must share quantum variables", loc)) else sigma_then

and typecheck_internal_par gamma internal_par =
  match internal_par with
  | { node = InternalPar internal_choice_list; loc } ->
    let sigma_list = List.map (typecheck_internal_choice gamma) internal_choice_list in
    check_par_intersection loc sigma_list

and typecheck_seq gamma seq =
  match seq with
  | { node = nod; loc } ->
    match nod with
    | Tau rest -> typecheck_internal_par gamma rest
    | Measure (qnames, var, rest) ->
        List.iter (fun qname -> 
          match typecheck_access gamma qname with
            | (TQuant, _) -> ()
            | (anyTyp, _) -> raise(TypeException("Cannot measure a " ^ show_typ anyTyp, loc))
        ) qnames;
        (* declare variable var of the type int 
           because the measurement returns an integer *)
        declare_var gamma var loc TInt;
        (* typecheck the rest of the program *)
        typecheck_internal_par gamma rest
    | QOp (qop, acclis, rest) -> 
      let given_types = List.map(fun acc -> typecheck_access gamma acc |> fst) acclis in
      let required_types = (match qop with
        | H | X | Y | Z -> [TQuant]
        | CX -> [TQuant; TQuant]) in
      (* ensure the given types are the SAME of the required *)
      let are_equal = List.equal (=) given_types required_types in
      if not are_equal then 
        raise(TypeException("Quantum operation used with invalid types or invalid number of arguments", loc))
      (* typecheck the rest of the program *)
      else typecheck_internal_par gamma rest
    | Recv (Chan(_, chantyp), var, rest) -> 
      (* declare variable var of the same type of the receiving channel *)
      declare_var gamma var loc chantyp;
      (* typecheck the rest of the program *)
      typecheck_internal_par gamma rest
    | Send (Chan(_, chantyp), exp) ->
      (* typecheck exp and check that its type is the channel's type *)
      let exptyp, qb_opt = typecheck_expr gamma exp in
      if chantyp <> exptyp
      then raise(TypeException("Cannot send a " ^ show_typ exptyp ^ " through a " ^ show_typ chantyp ^ " channel", loc));
      (match exptyp, qb_opt with
          TQuant, Some qb -> SigmaSet.of_list [(qb, true)]
        | _ -> SigmaSet.empty )
    | Discard qnames ->
      let discardedlist = List.map (fun qname -> 
        match typecheck_access gamma qname with
          | TQuant, Some qb -> (qb, true)
          | anyTyp, _ -> raise(TypeException("Cannot discard a " ^ show_typ anyTyp, loc))
      ) qnames in
      SigmaSet.of_list discardedlist

let typecheck_external_choice gamma external_choice =
  match external_choice with
  | { node = ExternalChoice(seq_list); loc } -> typecheck_choice gamma seq_list loc

let typecheck_external_par gamma external_par =
  match external_par with
  | { node = ExternalPar external_choice_list; loc } ->
    let sigma_list = List.map (typecheck_external_choice gamma) external_choice_list in
    check_par_intersection loc sigma_list |> ignore

let typecheck_program gamma prog =
  match prog with
  | Prog(external_par, restr) ->
      typecheck_external_par gamma external_par;
      typecheck_restr gamma restr

let typecheck (ast : program) =
  let gamma = Hashtbl.create 256 in
  typecheck_program gamma ast |> ignore;
  ast
