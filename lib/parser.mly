%{
  open Ast

  let build_node l n =
    { loc = Location.to_code_position(l); node = n }
%}

%token EOF
%token <string> ID
%token <int> INTEGER
%token <bool> BOOLEAN
%token <int> QBIT
%token PLUS MINUS
%token EQ GT LT GEQ LEQ NOT
%token AND OR
%token QMARK BANG PAR
%token QOP_H QOP_X QOP_Y QOP_I QOP_Z QOP_CX
%token INT_TYP BOOL_TYP QUANT_TYP
%token LPAREN RPAREN IF THEN ELSE DOT COMMA BACKSLASH COLON
%token MEASURE DISCARD TAU CHOICE

/*%nonassoc THEN
%nonassoc ELSE*/

/* lowest precedence */
%left AND
%left EQ GT LT GEQ LEQ
%left PLUS
%left OR
%nonassoc NOT
%nonassoc MINUS
/* highest precedence  */

%start program

%type <Ast.program> program
%type <Ast.external_par> external_par
%type <Ast.internal_choice> internal_choice
%type <Ast.internal_par> internal_par
%type <Ast.seq> seq
%type <Ast.expr> expr
%type <Ast.access> access

%%

program:
  external_par BACKSLASH restr_list EOF { Prog($1, $3) }

external_par:
| separated_nonempty_list(PAR, external_choice) 
    { build_node $loc (ExternalPar($1)) }

external_choice:
  LPAREN external_choice RPAREN { $2 }
| separated_nonempty_list(CHOICE, seq) { build_node $loc (ExternalChoice($1)) }

internal_choice:
  LPAREN internal_choice RPAREN  
    { $2 }
| seq 
    { build_node $loc (InternalChoice([$1])) }
| LPAREN seq CHOICE separated_nonempty_list(CHOICE, seq) RPAREN 
    { build_node $loc (InternalChoice($2::$4)) }
| IF expr THEN internal_par ELSE internal_par 
    { build_node $loc (IfThenElse($2, $4, $6)) }

internal_par:
  internal_choice 
    { build_node $loc (InternalPar([$1])) }
| LPAREN internal_choice PAR separated_nonempty_list(PAR, internal_choice) RPAREN 
    { build_node $loc (InternalPar($2::$4)) }

seq:
  TAU DOT internal_par 
    { build_node $loc (Tau($3)) }
| MEASURE LPAREN nonempty_access_list GT ID RPAREN DOT internal_par 
    { build_node $loc (Measure($3, VarName($5), $8)) }
| qop LPAREN nonempty_access_list RPAREN DOT internal_par 
    { build_node $loc (QOp($1, $3, $6)) }
| chan QMARK ID DOT internal_par 
    { build_node $loc (Recv($1, VarName($3), $5)) }
| chan BANG expr 
    { build_node $loc (Send($1, $3)) }
| DISCARD LPAREN access_list RPAREN 
    { build_node $loc (Discard($3)) }

access_list:
  separated_list(COMMA, access) { $1 }

nonempty_access_list:
  separated_nonempty_list(COMMA, access) { $1 }

access:
  ID    { build_node $loc (AccessVar(VarName($1))) }
| QBIT  { build_node $loc (AccessQBit($1)) }

expr:
  LPAREN expr RPAREN  { $2 }
| expr binop expr     { build_node $loc (BinaryOp($2, $1, $3)) }
| MINUS expr          { build_node $loc (UnaryOp(Neg, $2)) }
| NOT expr            { build_node $loc (UnaryOp(Not, $2)) }
| BOOLEAN             { build_node $loc (BLiteral($1)) }
| INTEGER             { build_node $loc (ILiteral($1)) }
| access              { build_node $loc (Access($1)) }

chan:
  LPAREN chan RPAREN  { $2 }
| ID COLON typ        { Chan($1, $3) }

restr_list:
  LPAREN separated_list(COMMA, chan) RPAREN { build_node $loc (Restr($2)) }

typ:
  INT_TYP     { TInt }
| BOOL_TYP    { TBool }
| QUANT_TYP   { TQuant }

%inline binop:
  PLUS  { Ast.Sum }
| AND   { Ast.And }
| OR    { Ast.Or }
| EQ    { Ast.Eq }
| LT    { Ast.Lt }
| GT    { Ast.Gt }
| LEQ   { Ast.Leq }
| GEQ   { Ast.Geq }

%inline qop:
  QOP_H   { H }
| QOP_X   { X }
| QOP_I   { I }
| QOP_Z   { Z }
| QOP_Y   { Y }
| QOP_CX  { CX }