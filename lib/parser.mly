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
%token EQ PAR PLUS
/* Other symbols */
%token SEMICOL
/*%token LPAREN RPAREN*/
/* Operators */
/*%token ADD SUB MULT DIV MOD ASSIGN
%token SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV
%token INCREMENT DECREMENT
%token GT LT GEQ LEQ NEQ
%token OR AND NOT*/
/* Other symbols */
/*%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token SEMICOL COMMA
%token AMPERSAND*/
/* Keywords */
/*%token INT CHAR VOID BOOL NULL
%token IF RETURN THEN ELSE FOR WHILE DO*/


/* ------ Precedence and associativity specification ------ */
/* Fix for the dangling-else conflict. Idea from: 
http://gallium.inria.fr/~fpottier/X/INF564/html/parser.mly.html */
/*%nonassoc THEN
%nonassoc ELSE

%right ASSIGN SHORTADD SHORTSUB SHORTMULT SHORTMOD SHORTDIV /* lowest precedence */
/*%left OR 
%left AND 
%left EQ NEQ
%nonassoc GT LT GEQ LEQ
%left ADD SUB 
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
    (* just one process *)
    simpleproc { $1 }
    (* one or more parallel processes *)
  | simpleproc PAR separated_nonempty_list(PAR, simpleproc) { build_node $loc (Ast.Par($1::$3)) }
;

simpleproc:
    seq { build_node $loc (Ast.Seq($1)) }
  (*| IF expr THEN proc ELSE proc
  | proc SLASH ID this is restriction *)
;

seq:
    (* just one seq *)
    simpleseq { $1 } 
    (* one or more seq sperated by plus *)
  | simpleseq PLUS separated_nonempty_list(PLUS, simpleseq) { build_node $loc (Ast.NonDeterm($1::$3)) }
;

simpleseq:
    ID { build_node $loc (Ast.Discard($1)) }
;