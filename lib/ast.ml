type typ = 
  | TInt
  | TBool
  | TQuant
[@@deriving show]

type binop =
  | Lt
  | Gt
  | Geq
  | Leq
  | Eq
  | Or
  | And
  | Sum
[@@deriving show]


type uop = 
  | Not
  | Neg
[@@deriving show]


type qop = 
  | H
  | X
  | I
  | Z
  | CX
[@@deriving show]


 type 'a annotated_node = {
  loc : Location.code_pos; [@opaque]
  node : 'a;
}
[@@deriving show]
(*
type 'a annotated_node = 'a
[@@deriving show]
*)

type var = VarName of string
[@@deriving show]

type chan = Chan of string * typ
[@@deriving show]


type access = access_node annotated_node
and access_node =
  | AccessVar of var
  | AccessQBit of int
[@@deriving show]

type expr = expr_node annotated_node
and expr_node = 
  | BinaryOp of binop * expr * expr 
  | UnaryOp of uop * expr 
  | ILiteral of int
  | BLiteral of bool
  | Access of access
[@@deriving show]

type seq = seq_node annotated_node
and seq_node = 
  | Tau of internal_par
  | Measure of access list * var * internal_par
  | QOp of qop * access list * internal_par
  | Recv of chan * var * internal_par
  | Send of chan * expr
  | Discard of access list
[@@deriving show]

and external_choice = external_choice_node annotated_node
and external_choice_node = ExternalChoice of seq list [@@deriving show]

and internal_choice = internal_choice_node annotated_node
and internal_choice_node = 
  | InternalChoice of seq list
  | IfThenElse of expr * internal_par * internal_par
[@@deriving show]

and external_par = external_par_node annotated_node
and external_par_node = ExternalPar of external_choice list [@@deriving show]

and internal_par = internal_par_node annotated_node
and internal_par_node = InternalPar of internal_choice list [@@deriving show]

type restr = restr_node annotated_node
and restr_node = Restr of chan list [@@deriving show]

type program = Prog of external_par * restr [@@deriving show]
