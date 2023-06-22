open Ast
open Location

exception TypeException of string * code_pos

module SigmaElement = struct
  type t = access_node * bool

  let compare (qbit1, _) (qbit2, _) = compare qbit1 qbit2
end

module SigmaSet = Set.Make (SigmaElement)

let declare_chan chantbl loc (Chan (cname, ctype)) =
  try
    let c = Hashtbl.find chantbl cname in
    if c = ctype then () (* skip, if the channel was already defined *)
    else
      raise
        (TypeException
           ( "channel '" ^ cname ^ "' previously declared of type " ^ show_typ c,
             loc ))
  with Not_found ->
    (* create variable with vtype in env *)
    Hashtbl.add chantbl cname ctype

let declare_var env (VarName var) loc vartype =
  match Hashtbl.find_opt env var with
  | Some v ->
      raise
        (TypeException
           ("variable '" ^ var ^ "' already declared of type " ^ show_typ v, loc))
  | None ->
      (* create variable with vtype in env *)
      Hashtbl.add env var vartype

let typecheck_access env access =
  match access with
  | { node = nod; loc } -> (
      match nod with
      | AccessVar (VarName var) -> (
          (* check that var exists and return its type *)
          match Hashtbl.find_opt env var with
          | Some TQuant -> (TQuant, Some (AccessVar (VarName var)))
          | Some ctype -> (ctype, None)
          | None ->
              raise (TypeException ("Variable '" ^ var ^ "' not declared", loc))
          )
      | AccessQBit qb ->
          (* ensure the qbit is higher than 1 or equal *)
          if qb > 0 then (TQuant, Some (AccessQBit qb))
          else
            raise
              (TypeException ("QBit index must be higher or equal to 1", loc)))

let rec typecheck_expr chantbl env expr =
  match expr with
  | { node = nod; loc } -> (
      match nod with
      | BinaryOp (binop, expr1, expr2) ->
          let typ1, _ = typecheck_expr chantbl env expr1 in
          let typ2, _ = typecheck_expr chantbl env expr2 in
          (* auxiliary function to raise meaningful exception *)
          let fail_fun opname typ1 typ2 =
            raise
              (TypeException
                 (opname ^ " " ^ show_typ typ1 ^ " and " ^ show_typ typ2, loc))
          in
          (* ensure all the operations are between the same types *)
          if typ1 <> typ2 then fail_fun "Invalid binary op between" typ1 typ2
          else
            let binoptyp =
              match binop with
              | Lt | Gt | Geq | Leq | Or | And ->
                  (* ensure comparison between booleans *)
                  if typ1 <> TBool then fail_fun "Cannot compare" typ1 typ2
                  else TBool
              (* ensure quality between booleans or integers *)
              | Eq ->
                  if typ1 = TQuant then fail_fun "Cannot compare" typ1 typ2
                  else TBool
              (* ensure sum between integers *)
              | Sum ->
                  if typ1 <> TInt then fail_fun "Cannot sum" typ1 typ2 else TInt
            in
            (binoptyp, Option.None)
      | UnaryOp (uop, expr1) ->
          let typ, _ = typecheck_expr chantbl env expr1 in
          let uoptyp =
            match uop with
            | Not ->
                if typ <> TBool then
                  raise (TypeException ("Cannot negate " ^ show_typ typ, loc))
                else TBool
            | Neg ->
                if typ <> TInt then
                  raise (TypeException ("Cannot negate " ^ show_typ typ, loc))
                else TInt
          in
          (uoptyp, Option.None)
      | ILiteral _ -> (TInt, Option.None)
      | BLiteral _ -> (TBool, Option.None)
      | Access acc -> typecheck_access env acc)

let typecheck_restr chantbl restr =
  match restr with
  | { node = Restr clist; loc } -> List.iter (declare_chan chantbl loc) clist

let rec typecheck_choice chantbl env seq_list loc =
  match seq_list with
  | [] ->
      raise
        (TypeException
           ("Non-deterministic choice needs at least 2 or more processes", loc))
  | head :: rest ->
      (* process first seq *)
      let sigma_head = typecheck_seq chantbl env head in
      (* process all the other seq *)
      let sigma_rest = List.map (typecheck_seq chantbl env) rest in
      (* for each sigma from the rest of seqs, check they are equal to the first one.
          in practice we ensure that all the sigmas are equal *)
      List.iter
        (fun sigma_i ->
          if not (SigmaSet.equal sigma_head sigma_i) then
            raise
              (TypeException
                 ( "Branches of a non-deterministic choice must share quantum \
                    variables",
                   loc )))
        sigma_rest;
      sigma_head

and check_par_intersection loc =
  List.fold_left
    (fun acc set ->
      let inters = SigmaSet.inter set acc in
      (* intersection must be empty *)
      if not (SigmaSet.is_empty inters) then
        raise
          (TypeException
             ("Parallel processes cannot share variables", loc));
      SigmaSet.union set acc)
    SigmaSet.empty

and typecheck_internal_choice chantbl env internal_choice =
  match internal_choice with
  | { node; loc } -> (
      match node with
      | InternalChoice seq_list -> typecheck_choice chantbl env seq_list loc
      | IfThenElse (expr, inter_node_1, inter_node_2) ->
          let guardtyp, _ = typecheck_expr chantbl env expr in
          if guardtyp <> TBool then
            raise (TypeException ("Only booleans are allowed as guard", loc));
          let sigma_then = typecheck_internal_par chantbl env inter_node_1 in
          let sigma_else = typecheck_internal_par chantbl env inter_node_2 in
          if not (SigmaSet.equal sigma_then sigma_else) then
            raise
              (TypeException
                 ("Branches of a conditional must share quantum variables", loc))
          else sigma_then)

and typecheck_internal_par chantbl env internal_par =
  match internal_par with
  | { node = InternalPar internal_choice_list; loc } ->
      let sigma_list =
        List.map (fun ch -> 
          let env_copy = Hashtbl.copy env in typecheck_internal_choice chantbl env_copy ch
        ) internal_choice_list
      in
      check_par_intersection loc sigma_list

and ensure_sent_or_discarded qlist sigma loc =
  (* fail if any qbit or quantum variable measured is never discarded or sent.
     This check will also ensure that all the measured quantum variables and
     qbits are in the sigma of the rest of the program *)
  List.iter
    (fun el ->
      match el with
      | AccessQBit qb -> (
          (* fail if the qbit is not discarded or sent (i.e. the second element is true) *)
          match SigmaSet.find_opt (el, true) sigma with
          | Some (_, true) -> ()
          | Some (_, false) | _ ->
              raise
                (TypeException
                   (Printf.sprintf "Qbit 'q%d' not discarded" qb, loc)))
      | AccessVar (VarName var) -> (
          (* fail if the quantum variable is not discarded or sent (i.e. the second element is true) *)
          match SigmaSet.find_opt (el, true) sigma with
          | Some (_, true) -> ()
          | Some (_, false) | _ ->
              raise
                (TypeException
                   ( Printf.sprintf "Quantum variable '%s' not discarded" var,
                     loc ))))
    qlist

and typecheck_seq chantbl env seq =
  match seq with
  | { node = nod; loc } -> (
      match nod with
      | Tau rest -> typecheck_internal_par chantbl env rest
      | Measure (qnames, var, rest) ->
          let tobe_checked =
            List.fold_left
              (fun acclis qname ->
                match typecheck_access env qname with
                | TQuant, Some acc -> acc :: acclis
                | TQuant, None -> acclis
                | anyTyp, _ ->
                    raise
                      (TypeException
                         ("Cannot measure a " ^ show_typ anyTyp, qname.loc)))
              [] qnames
          in
          (* declare variable var of the type int
             because the measurement returns an integer *)
          declare_var env var loc TInt;
          (* typecheck the rest of the program *)
          let sigma_rest = typecheck_internal_par chantbl env rest in
          (* ensure every quantum variable or qbit on which the measurement
             operation is applied it is sent or discarded later in the program *)
          ensure_sent_or_discarded tobe_checked sigma_rest loc;
          (* return the sigma of the rest of the program, which contains all the quantum variables and qbits *)
          sigma_rest
      | QOp (qop, acclis, rest) ->
          let tobe_checked =
            List.fold_left
              (fun accumlis qname ->
                match typecheck_access env qname with
                | TQuant, Some acc -> acc :: accumlis
                | TQuant, None -> accumlis
                | anyTyp, _ ->
                    raise
                      (TypeException
                         ( "Cannot apply quantum operation on a "
                           ^ show_typ anyTyp,
                           qname.loc )))
              [] acclis
          in
          let given_types =
            List.map (fun acc -> typecheck_access env acc |> fst) acclis
          in
          let required_types =
            match qop with
            | H | X | I | Z | Y -> [ TQuant ]
            | CX -> [ TQuant; TQuant ]
          in
          (* ensure the given types are the SAME of the required *)
          if not (List.equal ( = ) given_types required_types) then
            raise
              (TypeException
                 ( "Quantum operation used with invalid types or invalid \
                    number of arguments",
                   loc ));
          (match tobe_checked with
          | [ AccessQBit qb1; AccessQBit qb2 ] when qb1 = qb2 ->
              raise
                (TypeException ("Quantum operation used with same qbits", loc))
          | [ AccessVar (VarName var1); AccessVar (VarName var2) ]
            when var1 = var2 ->
              raise
                (TypeException
                   ("Quantum operation used with same quantum variables", loc))
          | _ -> ());
          (* typecheck the rest of the program *)
          let sigma_rest = typecheck_internal_par chantbl env rest in
          (* ensure every quantum variable or qbit on which the quantum
             operation is applied it is sent or discarded later in the program *)
          ensure_sent_or_discarded tobe_checked sigma_rest loc;
          sigma_rest
      | Recv (Chan (cname, chantyp), var, rest) ->
          declare_chan chantbl loc (Chan (cname, chantyp));
          (* declare variable var of the same type of the receiving channel *)
          declare_var env var loc chantyp;
          (* typecheck the rest of the program *)
          let sigma_rest = typecheck_internal_par chantbl env rest in
          (* if receiving quantum variable of qbit, then ensure
             it is sent or discarded later in the program *)
          if chantyp = TQuant then
            ensure_sent_or_discarded [ AccessVar var ] sigma_rest loc;
          SigmaSet.remove (AccessVar var, true) sigma_rest
      | Send (Chan (cname, chantyp), exp) -> (
          declare_chan chantbl loc (Chan (cname, chantyp));
          (* typecheck exp and check that its type is the channel's type *)
          let exptyp, qb_opt = typecheck_expr chantbl env exp in
          if chantyp <> exptyp then
            raise
              (TypeException
                 ( "Cannot send a " ^ show_typ exptyp ^ " through a "
                   ^ show_typ chantyp ^ " channel",
                   loc ));
          match (exptyp, qb_opt) with
          | TQuant, Some qbit_or_qvar ->
              SigmaSet.of_list [ (qbit_or_qvar, true) ]
          | _ -> SigmaSet.empty)
      | Discard qnames ->
          let discardedlist =
            List.map
              (fun qname ->
                match typecheck_access env qname with
                | TQuant, Some qb -> (qb, true)
                | anyTyp, _ ->
                    raise
                      (TypeException ("Cannot discard a " ^ show_typ anyTyp, loc)))
              qnames
          in
          SigmaSet.of_list discardedlist)

let typecheck_external_choice chantbl env external_choice =
  match external_choice with
  | { node = ExternalChoice seq_list; loc } -> typecheck_choice chantbl env seq_list loc

let typecheck_external_par chantbl env external_par =
  match external_par with
  | { node = ExternalPar external_choice_list; loc } ->
      let sigma_list =
        List.map (fun ch -> 
          let env_copy = Hashtbl.copy env in typecheck_external_choice chantbl env_copy ch
        ) external_choice_list
      in
      check_par_intersection loc sigma_list

let typecheck_program chantbl env prog =
  match prog with
  | Prog (external_par, restr) ->
      typecheck_restr chantbl restr;
      let global_sigmaset = typecheck_external_par chantbl env external_par in
      prog, global_sigmaset

let typecheck (ast : program) =
  let env = Hashtbl.create 256 in
  let chantbl = Hashtbl.create 256 in
  let ast, sigmaset = typecheck_program chantbl env ast in
  let qlist = SigmaSet.elements sigmaset in
  let maxq = List.fold_left (fun curr_max elem ->
    match elem with
    | (AccessQBit(i), _) -> max i curr_max
    | _ -> curr_max
  ) 0 qlist in
  ast, maxq
