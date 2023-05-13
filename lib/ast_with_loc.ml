type binop =
  | Leq
  | Or
[@@deriving show]


type uop = 
  | Not
[@@deriving show]


type qop = 
  | HADAMARD (* todo *)
[@@deriving show]


type 'a annotated_node = {
  loc : Location.code_pos; [@opaque]
  node : 'a;
}
[@@deriving show]


type var = string [@@deriving show]


type chan = string [@@deriving show]


type qvar = int [@@deriving show]
type qvarlist = QVarList of qvar list [@@deriving show]


type expr = expr_node annotated_node
and expr_node = 
  | BinaryOp of binop * expr * expr 
  | UnaryOp of uop * expr 
  | ILiteral of int
  | BLiteral of bool
  | AccessVar of var
  | AccessQVar of qvar
[@@deriving show]


(*
  Types definition forms a cycle:
  Par_prime -> Seq -> Choice -> Par -> Par_prime   
*)
type seq = seq_node annotated_node
and seq_node = 
  | Tau of par_prime
  | Measure of qvarlist * var * par_prime
  | Eps of qop * qvarlist * par_prime
  | Recv of chan * var * par_prime
  | Send of chan * expr
  | Discard of qvarlist
[@@deriving show]

and choice = choice_node annotated_node
and choice_node = Choice of seq list [@@deriving show]

and par = par_node annotated_node
and par_node = Par of choice list [@@deriving show]

and par_prime = par_prime_node annotated_node
and par_prime_node =
  | IfThenElse of expr * par_prime * par_prime
  | ParPrime of par
[@@deriving show]


type program = Prog of par * chan list [@@deriving show]