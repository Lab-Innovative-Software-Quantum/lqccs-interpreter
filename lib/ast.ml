type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
[@@deriving show]

type uop = 
    Neg 
  | Not
[@@deriving show]

type identifier = string [@@deriving show]

type 'a annotated_node = {
  loc : Location.code_pos; [@opaque]
  node : 'a;
}
[@@deriving show]

type typ =
  | TypI (* Type int *)
  | TypB (* Type bool *)
[@@deriving show]

type expr = expr_node annotated_node

and expr_node = 
  | Access of identifier (* access variable *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator  *)
  | ILiteral of int (* Integer literal  *)
  | BLiteral of bool (* Bool literal    *)
[@@deriving show]

type seq = seq_node annotated_node

and seq_node = 
  | Discard of string
  | NonDeterm of seq list
[@@deriving show]

type proc = proc_node annotated_node

and proc_node = 
  | Seq of seq
  | Par of proc list
  | IfThenElse of expr * proc * proc
  (*| Restr of proc * string list*)
[@@deriving show]

(*type proc_decl = {
  name : string;
  proc : proc;
}
[@@deriving show]

type decl = decl_node annotated_node

and decl_node = Procdecl of proc_decl
[@@deriving show]*)

type program = Prog of proc [@@deriving show]
