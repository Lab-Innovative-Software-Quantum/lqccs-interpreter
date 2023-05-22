/*
* LqCCS Parser specification
*/

%{
  (* Auxiliary definitions *)
  open Ast

  let build_node l n =
    { loc = Location.to_code_position(l); node = n }
%}


/* ---------------------------- Tokens declarations --------------------------- */
%token EOF
%token <string> ID
%token <int>  INTEGER
%token <bool> BOOLEAN
%token <char> CHARACTER
/*  */
/* Operators */
%token PAR PLUS
%token EQ GT LT GEQ LEQ NEQ
/* Other symbols */
%token LPAREN RPAREN
%token COMMA
/* Keywords */
%token IF THEN ELSE

/* Operators */
/*
%token ADD SUB MULT DIV MOD ASSIGN
%token SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV
%token INCREMENT DECREMENT
%token GT LT GEQ LEQ NEQ
%token OR AND NOT*/
/* Other symbols */
/*
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token SEMICOL COMMA
%token AMPERSAND*/
/* Keywords */
/*%token INT CHAR VOID BOOL NULL
%token IF RETURN THEN ELSE FOR WHILE DO*/

/*%right ASSIGN SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV /* lowest precedence */
%left EQ NEQ
%nonassoc GT LT GEQ LEQ

/*%left ADD SUB 
%left MULT DIV MOD
%nonassoc NOT AMPERSAND
%nonassoc NEG
%nonassoc LBRACKET    /* highest precedence  */

/* Starting symbol */
%start program
%type <Ast.program> program    /* the parser returns a Ast.program value */
%%

/* -------------------------- Grammar specification --------------------------- */
program:
    proc EOF    { Ast.Prog($1) }
;

(*decl:
    ID EQ proc SEMICOL { build_node $loc (Ast.Procdecl({ name = $1; proc = $3 })) }
;*)

proc:
    parlist { build_node $loc (Ast.Par($1)) }
  | simpleproc  { $1 }
;

parlist:
    simpleproc PAR simpleproc { [$1; $3] }
  | simpleproc PAR parlist { $1::$3 }
  | LPAREN simpleproc PAR simpleproc RPAREN PAR parlist { $2::$4::$7 }
;

simpleproc:
    seq                         { build_node $loc (Ast.Seq($1)) }
  | LPAREN simpleproc RPAREN    { $2 }
  | IF expr THEN LPAREN proc RPAREN ELSE LPAREN proc RPAREN { build_node $loc (Ast.IfThenElse($2, $5, $9)) }
  (*| proc SLASH ID this is restriction *)
;

seq:
    (* just one seq *)
    simpleseq { $1 } 
    (* one or more seq sperated by plus *)
  | simpleseq PLUS separated_nonempty_list(PLUS, simpleseq) { build_node $loc (Ast.NonDeterm($1::$3)) }
;

simpleseq:
    ID  { build_node $loc (Ast.Discard($1)) }
;

expr:
    lexpr   { build_node $loc $1 }
  | rexpr   { build_node $loc $1 }
;

lexpr:
    ID                    { Ast.Access($1) }
  | LPAREN lexpr RPAREN   { $2 }
;

rexpr:
    aexpr                   { $1 }
  | expr binop expr         { Ast.BinaryOp($2, $1, $3) }
;

aexpr:
    INTEGER               { Ast.ILiteral($1) }
  | BOOLEAN               { Ast.BLiteral($1) }
  | LPAREN rexpr RPAREN   { $2 }
;

%inline binop: // inline to fix shift/reduce conflicts
  | EQ    { Ast.Equal }
  | NEQ   { Ast.Neq }
  | LT    { Ast.Less }
  | GT    { Ast.Greater }
  | LEQ   { Ast.Leq }
  | GEQ   { Ast.Geq }
;