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
  | PreIncr
  | PostIncr
  | PreDecr
  | PostDecr
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

and expr_node = string
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
  (*| Restr of proc * string list
  | IfThenElse of expr * proc * proc*)
[@@deriving show]

type proc_decl = {
  name : string;
  proc : proc;
}
[@@deriving show]

(*type decl = decl_node annotated_node

and decl_node = Procdecl of proc_decl
[@@deriving show]*)

type program = Prog of proc [@@deriving show]
