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
  | TypC (* Type char *)
  | TypA of typ * int option (* Array type *)
  | TypP of typ (* Pointer type  *)
  | TypV (* Type void  *)
  | TypNull (* Type of null  *)
[@@deriving show]

and expr = expr_node annotated_node

and expr_node =
  | Access of access (* x  or  *p  or  a[e]  *)
  | Assign of access * expr (* x=e  or  *p=e  or  a[e]=e   *)
  | Addr of access (* &x   or  &*p   or  &a[e]  *)
  | ILiteral of int (* Integer literal  *)
  | CLiteral of char (* Char literal    *)
  | BLiteral of bool (* Bool literal    *)
  | Null
  | UnaryOp of uop * expr (* Unary primitive operator  *)
  | BinaryOp of binop * expr * expr (* Binary primitive operator  *)
  | Call of identifier * expr list (* Function call f(...)    *)
[@@deriving show]

and access = access_node annotated_node

and access_node =
  | AccVar of identifier (* Variable access    x  *)
  | AccDeref of expr (* Pointer dereferencing  *p *)
  | AccIndex of access * expr (* Array indexing   a[e] *)
[@@deriving show]

and stmt = stmt_node annotated_node

and stmt_node =
  | If of expr * stmt * stmt (* Conditional    *)
  | While of expr * stmt (* While loop     *)
  | Expr of expr (* Expression statement   e;  *)
  | Return of expr option (* Return statement  *)
  | Block of stmtordec list (* Block: grouping and scope *)
[@@deriving show]

and var_decl = {
  typ : typ;
  vname : string;
  init : expr option;
}
[@@deriving show]

and stmtordec = stmtordec_node annotated_node

and stmtordec_node =
  | Dec of var_decl (* Local variable declaration  *)
  | Stmt of stmt (* A statement   *)
[@@deriving show]

type proc_decl = {
  name : string;
}
[@@deriving show]

type decl = decl_node annotated_node

and decl_node = Procdecl of proc_decl
[@@deriving show]

type program = Prog of decl list [@@deriving show]
