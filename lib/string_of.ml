open Ast

let typ_to_str typ =
  match typ with TInt -> "int" | TBool -> "bool" | TQuant -> "quant"

let binop_to_str op =
  match op with
  | Sum -> "+"
  | Eq -> "="
  | Gt -> ">"
  | Geq -> ">="
  | Lt -> "<"
  | Leq -> "<="
  | And -> "AND"
  | Or -> "OR"

let uop_to_str op = match op with Not -> "NOT " | Neg -> "-"

let string_of_list printer ?(separator = ", ") ?(start_char = "")
    ?(end_char = "") lst =
  let buffer = Buffer.create 32 in
  let rec print_elements = function
    | [] -> ()
    | [ x ] -> Buffer.add_string buffer (printer x)
    | x :: xs ->
        Buffer.add_string buffer (printer x);
        Buffer.add_string buffer separator;
        print_elements xs
  in
  Buffer.add_string buffer start_char;
  print_elements lst;
  Buffer.add_string buffer end_char;
  Buffer.contents buffer

let rec string_of_acclist acclist =
  string_of_list string_of_access acclist

and string_of_access acc =
  match acc.node with
  | AccessVar (VarName vname) -> Printf.sprintf "%s" vname
  | AccessQBit qb -> Printf.sprintf "q%d" qb

let rec string_of_program (Prog (ext_par, _)) =
  match ext_par.node with
  | ExternalPar external_choice_list ->
      string_of_list
        (fun ext_choice ->
          match ext_choice.node with
          | ExternalChoice seq_list -> string_of_choices seq_list)
        ~separator:" || " external_choice_list

and string_of_expr expr =
  match expr.node with
  | ILiteral lit -> Printf.sprintf "%d" lit
  | BLiteral lit -> Printf.sprintf "%b" lit
  | BinaryOp (op, e1, e2) ->
      Printf.sprintf "%s %s %s" (string_of_expr e1) (binop_to_str op)
        (string_of_expr e2)
  | UnaryOp (op, e1) ->
      Printf.sprintf "%s%s" (uop_to_str op) (string_of_expr e1)
  | Access acc -> string_of_access acc

and string_of_seq seq =
  match seq.node with
  | Tau rest -> Printf.sprintf "Tau.%s" (string_of_internal_par rest)
  | Measure (acclist, VarName vname, rest) ->
      Printf.sprintf "M(%s > %s).%s"
        (string_of_acclist acclist)
        vname
        (string_of_internal_par rest)
  | QOp (op, acclist, rest) ->
      let qopstr =
        match op with X -> "X" | I -> "I" | Z -> "Z" | CX -> "CX" | H -> "H"
      in
      Printf.sprintf "%s(%s).%s" qopstr
        (string_of_acclist acclist)
        (string_of_internal_par rest)
  | Recv (Chan (cname, typ), VarName vname, rest) ->
      Printf.sprintf "%s:%s?%s.%s" cname (typ_to_str typ) vname
        (string_of_internal_par rest)
  | Send (Chan (cname, typ), expr) ->
      Printf.sprintf "%s:%s!%s" cname (typ_to_str typ) (string_of_expr expr)
  | Discard acclist ->
      Printf.sprintf "Discard(%s)" (string_of_acclist acclist)

and string_of_internal_par int_par =
  match int_par.node with
  | InternalPar int_choice_list ->
      string_of_list
        (fun int_choice ->
          match int_choice.node with
          | InternalChoice seq_list -> string_of_choices seq_list
          | IfThenElse (quard, then_branch, else_branch) ->
              Printf.sprintf "if %s then %s else %s"
                (string_of_expr quard)
                (string_of_internal_par then_branch)
                (string_of_internal_par else_branch))
        int_choice_list ~separator:" || "

and string_of_choices seq_list =
  string_of_list string_of_seq seq_list ~separator:" ++ "