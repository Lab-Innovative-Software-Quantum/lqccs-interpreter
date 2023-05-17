type binop =
  | Lt
  | Gt
  | Geq
  | Leq
  | Eq
  | Or
  | And
[@@deriving show]


type uop = 
  | Not
  | Neg
[@@deriving show]


type qop = 
  | H
  | X
  | Y
  | Z
  | CX
[@@deriving show]


(* type 'a annotated_node = {
  loc : Location.code_pos; [@opaque]
  node : 'a;
}
[@@deriving show]*)
type 'a annotated_node = 'a
[@@deriving show]


type var = VarName of string [@@deriving show]

type chan = Chan of string * ctype 
and ctype = 
  | TInt
  | TBool
  | TQuant
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

(*
  Types definition forms a cycle:
  Par_prime -> Seq -> Choice -> Par -> Par_prime   
*)
type seq = seq_node annotated_node
and seq_node = 
  | Tau of par
  | Measure of access list * var * par
  | QOp of qop * access list * par
  | Recv of chan * var * par
  | Send of chan * expr
  | Discard of access list
[@@deriving show]

and choice = choice_node annotated_node
and choice_node = Choice of seq list [@@deriving show]

and par_top = par_top_node annotated_node
and par_top_node = Par of choice list [@@deriving show]

and par = par_node annotated_node
and par_node =
  | IfThenElse of expr * par * par
  | ParTopLevel of par_top
[@@deriving show]

type restr = restr_node annotated_node
and restr_node = Restr of chan list [@@deriving show]

type program = Prog of par_top * restr [@@deriving show]

(*
discard = Discard(AccessQBit(0))
ifthenelse = 
measurement = Measure([AccessQBit(0)], VarName("x"), ParTopLevel(Par([Choice([Seq(ifthenelse)]), Choice([Seq(discard)])]))))
veroverissimo = QOp(H, [AccessQBit(0)], ParTopLevel(Par([Choice([Seq(measurement)])]))))

Meas().Parall()

Prog((Par([Choice([veroverissimo])]), Restr([])))

*)