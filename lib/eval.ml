open Ast
open Symbol_table
open Typechecker
open Qop
open String_of

let debug = true

exception EvalException of Location.code_pos * string

type qstate = Complex.t list
type conf = Conf of qstate * Ast.program * float
type distr = Distribution of conf list

type value = Int of int | Bool of bool | QBit of int
type memory = Memory of value Symbol_table.t * (value * bool) Symbol_table.t
type process = Process of memory * conf
type running_distr = RunDistr of process list
type proc_state = CanAdvance of internal_par | Waiting

let pretty_string_of_float number =
  let string_number = string_of_float number in
  let str_length = String.length string_number in
  if str_length > 0 && String.get string_number (str_length - 1) = '.' then
    String.sub string_number 0 (str_length - 1)
  else string_number

let string_of_qstate qst =
  string_of_list string_of_complex ~start_char:"[" ~end_char:"]" qst

let string_of_conf (Conf (qst, prg, prob)) =
  Printf.sprintf "(%s, %s, %s)" (string_of_qstate qst) (string_of_program prg)
    (pretty_string_of_float prob)

let string_of_distribution (Distribution confs) =
  string_of_list string_of_conf confs ~start_char:"[" ~end_char:"]"

let eval_binop (bop : binop) (v1 : value) (v2 : value) (loc : Location.code_pos)
    : value =
  match (bop, v1, v2) with
  | Sum, Int a, Int b -> Int (a + b)
  | Eq, Int a, Int b -> Bool (a == b)
  | Eq, Bool a, Bool b -> Bool (a == b)
  | Gt, Int a, Int b -> Bool (a > b)
  | Geq, Int a, Int b -> Bool (a >= b)
  | Lt, Int a, Int b -> Bool (a < b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _ -> raise (TypeException ("Type error", loc))

let eval_uop (op : uop) (v : value) (loc : Location.code_pos) : value =
  match (op, v) with
  | Not, Bool lit -> Bool (not lit)
  | Neg, Int lit -> Int (-lit)
  | _ -> raise (TypeException ("Type error", loc))

let eval_access symtbl access =
  match access with
  | { node = nod; loc } -> (
      match nod with
      | AccessVar var -> (
          match lookup var symtbl with
          | Some content -> content
          | None -> raise (TypeException ("Variable not declared", loc)))
      | AccessQBit qb -> QBit qb)

let get_qindexes ql symtbl =
  List.map
    (fun acc ->
      (* ensure only variables referring to qbits (or just qbits) are used *)
      match eval_access symtbl acc with
      | Bool _ -> raise (TypeException ("Cannot measure/qop a bool", acc.loc))
      | Int _ -> raise (TypeException ("Cannot measure/qop a int", acc.loc))
      | QBit qb -> qb)
    ql

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
      | ILiteral lit -> Int lit
      | BLiteral lit -> Bool lit
      | Access acc -> eval_access symtbl acc)

let eval_seq seq proc =
  let (Process (Memory (symtbl, channels), Conf (qst, prg, prob))) = proc in
  match seq with
  | { node = nod; _ } -> (
      match nod with
      | Tau rest -> [(proc, CanAdvance rest)]
      | Measure (acclist, VarName vname, rest) ->
          (* get the list of qbits *)
          let q_indexes = get_qindexes acclist symtbl in
          (* perform measurement and get the resulting distribution list *)
          let res_list = measure qst q_indexes in
          (* for each result and its quantum state and probability *)
          List.fold_left (fun acc (new_qst, res, m_prob) ->
            if m_prob = 0.0 then acc
              (* if the measurement is 0, skip this configuration *)
            else
              (* build a new scope in the symbol table *)
              let new_symtbl = begin_block symtbl in
              (* build a new scope in the channels symbol table *)
              let new_channels = begin_block channels in
              (* put the given result in the new symbol table *)
              add_entry (VarName vname) (Int res) new_symtbl |> ignore;
              (* return the new quantum state, the rest of the program,
                  the new probability and the new symbol table *)
              (Process(Memory(new_symtbl, new_channels), Conf(new_qst, prg, prob *. m_prob)), CanAdvance rest)::acc
          ) [] res_list
      | QOp (op, acclist, rest) ->
          (* get the list of qbits *)
          let q_indexes = get_qindexes acclist symtbl in
          let firstqbit = List.hd q_indexes in
          let new_qst =
            match op with
            | H -> qop_h qst firstqbit
            | X -> qop_x qst firstqbit
            | I -> qop_i qst firstqbit
            | Z -> qop_z qst firstqbit
            | Y -> qop_y qst firstqbit
            | CX ->
                let sndqbit = List.hd (List.tl q_indexes) in
                qop_cx qst firstqbit sndqbit
          in
          [(Process(Memory(symtbl, channels), Conf(new_qst, prg, prob)), CanAdvance rest)]
      (* | Recv (Chan (cname, _), vname, rest) -> (
          (* check if the channel has a pending value *)
          match lookup (VarName cname) channels with
          | None ->
              (* nobody sent anything on the channel *)
              [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Waiting)]
          | Some (_, true) ->
              (* another process already received the pending value *)
              [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Waiting)]
          | Some (v, false) ->
              (* replace the pending value with it and true *)
              remove_entry (VarName cname) channels |> ignore;
              add_entry (VarName cname) (v, true) channels |> ignore;
              (* add the value received to the symbol table *)
              add_entry vname v symtbl |> ignore;
              [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), CanAdvance rest)])
      | Send (Chan (cname, _), expr) -> (
          (* check if the channel has a pending value *)
          match lookup (VarName cname) channels with
          | Some (v, true) ->
              (* if the channel has a pending value, who was received *)
              let this_v = eval_expr symtbl expr in
              if v = this_v then
                let _ = remove_entry (VarName cname) channels in
                (* there isn't nothing else (None) and this process is ended (true) *)
                [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Ended)]
              else
                [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Waiting)]
          | Some (_, false) ->
              (* if the channel has a pending value, who was NOT received *)
              [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Waiting)]
          | None ->
              (* if the channel has NOT a pending value *)
              (* eval expr to obtain the value *)
              let v = eval_expr symtbl expr in
              (* add a pending value on the channel *)
              add_entry (VarName cname) (v, false) channels |> ignore;
              (* return that this process is completed but waiting *)
              [(Process(Memory(symtbl, channels), Conf(qst, prg, prob)), Waiting)]
              (* return None such that the caller knows this process has ended *)
          ) *)
      | _ ->
        [(proc, Waiting)])

let rec cart_prod currlis lists =
  match lists with
  | [] -> [ List.rev currlis ]
  | first_list :: other_lists ->
      List.fold_left
        (fun acc elem ->
          let this_res = cart_prod (elem :: currlis) other_lists in
          List.append this_res acc)
        [] (List.rev first_list)

(* return a program with the given external choices in parallel *)
let prog_of_par ext_choices restr loc =
  Prog ({ node = ExternalPar ext_choices; loc }, restr)

(* get the external par of the given process *)
let extpar_of_proc (Process (Memory (_, _), Conf (_, Prog (ext_par, _), _))) =
  ext_par

let internal_par_to_external symtbl intpar =
  let rec recurse acc intpar =
    match intpar.node with
    | InternalPar intchoices ->
        List.fold_left
          (fun acc2 intchoice ->
            match intchoice.node with
            | IfThenElse (guard, then_branch, else_branch) -> (
                match eval_expr symtbl guard with
                | Bool true -> recurse acc2 then_branch
                | Bool false -> recurse acc2 else_branch
                | _ -> raise (TypeException ("Invalid guard type", guard.loc)))
            | InternalChoice seqlist ->
                { node = ExternalChoice seqlist; loc = intchoice.loc } :: acc2)
          acc intchoices
  in
  let extpar = ExternalPar (recurse [] intpar) in
  { node = extpar; loc = intpar.loc }

(* (A + B) || (C + D) || E || (F + G)
    ^   
    (recvB.dopoRecv + altro1) || sendB || (F + G) -> dopoRecv || (F + G) 
*)
let rec choices_to_processes res before_lis external_choice_list proc = 
  let (Process
  (Memory (symtbl, channels), Conf (qst, Prog (extpar, restr), prob))) = proc in
  match external_choice_list with
  | [] -> res
  (* Tau.Discard(q1) ++ Tau.Discard(q1) || Tau.Discard(q2) ++ Tau.Discard(q2) \ () *)
  (* | { node = ExternalChoice(_::[]); _}::restofchoices -> 
    choices_to_processes res ((List.hd external_choice_list)::before_lis) restofchoices proc  *)
  | { node = ExternalChoice(seqlis); loc }::restofchoices -> 
    (* for each seq into seqlis *)
    let new_choices_list = List.fold_left (fun new_choices_acc seq ->
      (match seq.node with
      (* if the current seq is a recv, find a valid send *)
      | Recv(Chan(recv_cname, _), vname, after_recv) -> 
          let (new_choice_lis, found) = List.fold_left (fun (acc2, found) choice ->
            if found then (choice::acc2, found) else
            match choice.node with
            | ExternalChoice(otherseqlis) ->
              (* find a valid send *)
              let send_opt = List.find_opt (fun otherseq ->
                (match otherseq.node with
                | Send(Chan(send_cname, _), _) when send_cname = recv_cname -> true
                | _ -> false)
              ) otherseqlis in
              (* if a send is found, perform matching by computing the value to be received *)
              (match send_opt with
              | Some({ node = Send(_, expr); _ }) ->
                let send_value = eval_expr symtbl expr in
                (* add the value received to the symbol table *)
                add_entry vname send_value symtbl |> ignore;
                let { node = ExternalPar(choicelis); _ } = internal_par_to_external symtbl after_recv in
                (* valid send found, do not keep this choice because data was sent *)
                (List.append choicelis acc2, true)
              | _ -> (* valid send not found, keep this choice *)
                (choice::acc2, found))
          ) ([], false) restofchoices
        in
        if not found then new_choices_acc else
        let extbefore = List.append before_lis new_choice_lis in
        extbefore::new_choices_acc
        (* (List.append extbefore restofchoices)::acc 
        (c:int!10 ++ a:int!30) || c:int?x.Discard(q1) || a:int?x.o:int!x \ ()   
        (c:int!10 ++ a:int!30) || c:int?x.Discard(q1) || (o:int!50 ++ Tau.Discard()) \ ()   
        *)
      (* if the current seq is a send, find a valid recv *)
      | Send(Chan(send_cname, _), sendexpr) -> 
        let (new_choice_lis, found) = List.fold_left (fun (acc2, found) choice ->
          if found then (choice::acc2, found) else
          match choice.node with
          | ExternalChoice(otherseqlis) ->
            (* find a valid recv *)
            let recv_opt = List.find_opt (fun otherseq ->
              (match otherseq.node with
              | Recv(Chan(recv_cname, _), _, _) when recv_cname = send_cname -> true
              | _ -> false)
            ) otherseqlis in
            (* if a recv is found, perform matching by computing the value to be received *)
            (match recv_opt with
              | Some({ node = Recv(_, vname, after_recv); _ }) ->
                let send_value = eval_expr symtbl sendexpr in
                (* add the value received to the symbol table *)
                add_entry vname send_value symtbl |> ignore;
                let { node = ExternalPar(choicelis); _ } = internal_par_to_external symtbl after_recv in
                (* valid send found, do not keep this choice because data was sent *)
                (List.append choicelis acc2, true)
              | _ -> (* valid recv not found, keep this choice *)
                (choice::acc2, found))
        ) ([], false) restofchoices
        in
        if not found then new_choices_acc else
        let extbefore = List.append before_lis new_choice_lis in
        extbefore::new_choices_acc
        (* (List.append extbefore restofchoices)::acc *)
      | Discard([]) -> new_choices_acc (* ignore empty discards *)
      (* | Discard(_) -> new_choices_acc *)
      | _ -> (* pick this element and create a new branch *)
        (match seqlis with
        | [] | _::[] -> new_choices_acc
        | _ ->
          let extbefore = List.append before_lis [{ node = ExternalChoice([seq]); loc }] in
          (List.append extbefore restofchoices)::new_choices_acc))
    ) [] seqlis in
    (* transforms inner list from new_choices_list into processes *)
    let new_distr_lis = List.map
    (fun ext_choices ->
      Process
        ( Memory (symtbl, begin_block channels),
          Conf (qst, prog_of_par ext_choices restr extpar.loc, prob) ))
          new_choices_list in
    let new_res = if new_distr_lis = res then res else (List.append new_distr_lis res) in
    choices_to_processes new_res ((List.hd external_choice_list)::before_lis) restofchoices proc

let debug_distributions title distributions =
  if not debug then ()
  else (
    Printf.printf "\n[DEBUG] %s:\n" title;
    List.iter
      (fun (RunDistr proclist) ->
        let distr =
          Distribution (List.map (fun (Process (_, cnf)) -> cnf) proclist)
        in
        Printf.printf "%s\n" (string_of_distribution distr))
      distributions;
    Printf.printf "\n")

let eval_process proc =
  let (Process (_, Conf (_, Prog (ext_par, restr), _))) = proc in
  let parallel_processes =
    match ext_par.node with
    | ExternalPar external_choice_list -> List.rev external_choice_list
  in
  match parallel_processes with
  | [] ->
      raise
        (EvalException (ext_par.loc, "Unexpected parallel without any process"))
  | extchoice :: rest ->
      let new_processes =
        match extchoice.node with
        | ExternalChoice [] ->
            raise
              (EvalException (extchoice.loc, "Unexpected empty external choice"))
        | ExternalChoice [ seq ] ->
            eval_seq seq proc
        | _ -> [(proc, Waiting)]
      in
      let reslis =
        List.fold_left
          (fun acc (parallel_proc, proc_state) ->
            match proc_state with
            (* | Ended ->
                if rest = [] then acc
                else
                  let new_ext_par =
                    { node = ExternalPar rest; loc = ext_par.loc }
                  in
                  let new_prog = Prog (new_ext_par, restr) in
                  Process (mem, Conf (qst, new_prog, prob)) :: acc *)
            | Waiting ->
                let (Process (mem, Conf (qst, _, prob))) = parallel_proc in
                let new_ext_par =
                  {
                    node = ExternalPar (List.append rest [ extchoice ]);
                    loc = ext_par.loc;
                  }
                in
                Process (mem, Conf (qst, Prog (new_ext_par, restr), prob))
                :: acc
            | CanAdvance intpar ->
                let (Process
                      (Memory (symtbl, channels), Conf (new_qst, _, new_prob)))
                    =
                  parallel_proc
                in
                let new_ext_par = internal_par_to_external symtbl intpar in
                let new_ext_par =
                  match new_ext_par.node with
                  | ExternalPar new_ext_choices ->
                      let newextpar =
                        ExternalPar (List.append rest new_ext_choices)
                      in
                      { node = newextpar; loc = new_ext_par.loc }
                in
                let new_prog = Prog (new_ext_par, restr) in
                let new_proc =
                  Process
                    ( Memory (symtbl, channels),
                      Conf (new_qst, new_prog, new_prob) )
                in
                new_proc :: acc)
          [] new_processes
      in
      reslis

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let rec eval_program distributions =
  debug_distributions "BEFORE PREPROCESSING" distributions;

  let after_preprocessing =
    List.fold_left
      (fun acc (RunDistr proclist) ->
        let processes =
          List.map
            (fun proc ->
              match extpar_of_proc proc with
              | { node = ExternalPar external_choice_list; _ } ->
                  choices_to_processes [] [] external_choice_list proc)
            proclist
        in

        let cp = cart_prod [] processes in
        let distrlis = List.map (fun proclis -> RunDistr proclis) cp in
        List.append distrlis acc)
      [] distributions
  in
  let after_preprocessing = List.rev after_preprocessing in
  
  debug_distributions "APPLIED PREPROCESSING" after_preprocessing;
  
  if after_preprocessing = [] then distributions else (

  let after_one_step =
    List.fold_left
      (fun accdistr (RunDistr proclist) ->
        let new_proc_lis =
          List.fold_left
            (fun acc proc ->
              let new_processes = eval_process proc in

              if new_processes <> [] then List.append new_processes acc else acc)
            [] proclist
        in
        let new_proc_lis = List.rev new_proc_lis in
        if new_proc_lis = [] then accdistr
        else RunDistr new_proc_lis :: accdistr)
      [] after_preprocessing
  in
  let after_one_step = List.rev after_one_step in

  debug_distributions "PERFORMED ONE STEP" after_one_step;
  
  eval_program after_one_step
  )

(* transforms the given value into the equivalent AST node *)
let value_to_ast value loc =
  {
    node =
      (match value with
      | Int lit -> ILiteral lit
      | Bool lit -> BLiteral lit
      | QBit qb -> Access { node = AccessQBit qb; loc });
    loc;
  }

(* given a configuration returns the same one but replaces the variables 
   with their values whenever it is possible *)
let enhance_conf symtbl (Conf(qst, Prog(ext_par, restr), prob)) =
  let new_ext_par = (
    (match ext_par.node with
    | ExternalPar(extchoices) -> { node = ExternalPar(
        List.map (fun choice ->
          match choice.node with
          | ExternalChoice(seqlis) -> { node = ExternalChoice(
            List.map (fun seq ->
              { node = (match seq.node with
                | Send(ch, expr) ->
                  let value = eval_expr symtbl expr in
                  Send(ch, value_to_ast value expr.loc)
                | Discard(acclis) ->
                  let acclis_val = List.map (fun acc ->
                    let value = eval_access symtbl acc in
                    match value with
                    | QBit(qb) -> { node = AccessQBit(qb); loc = acc.loc }
                    | _ -> acc
                  ) acclis in
                  Discard(acclis_val)
                | _ -> seq.node
                ); loc = seq.loc }
            ) seqlis
          ); loc = choice.loc }
        ) extchoices
      ); loc = ext_par.loc }
    )
  ) in
  (Conf(qst, Prog(new_ext_par, restr), prob))

let eval (prog : program) (qmax : int) =
  (* build empty symbol table *)
  let symtbl = begin_block empty_table in
  (* build empty channels symbol table *)
  let channels = begin_block empty_table in
  (* build the quantum state *)
  let qst = List.init (pow 2 qmax) (fun _ -> Complex.zero) in
  let qst = Complex.one :: List.tl qst in
  (* build the starting distribution that is an external par *)
  let starting_distr =
    RunDistr [ Process (Memory (symtbl, channels), Conf (qst, prog, 1.0)) ]
  in
  (* evaluate the starting process with the starting configuration *)
  let ending_distr = eval_program [ starting_distr ] in
  List.map
    (fun (RunDistr proclist) ->
      Distribution
        (List.map
           (fun (Process (Memory (symtbl, _), cnf)) -> enhance_conf symtbl cnf)
           proclist))
    ending_distr
