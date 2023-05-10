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
    tdlist = list(decl) EOF    { Ast.Prog(tdlist) }
  (*| EOF                             { Ast.Prog([]) } *)
;

decl:
    ID EQ proc SEMICOL { build_node $loc (Ast.Procdecl({ name = $1; proc = $3 })) }
;

parlist:
     { build_node $loc (Ast.Par($1)) }
;

proclist:
    separated_nonempty_list(PLUS, seq) { build_node $loc (Ast.NonDeterm($1)) }
    separated_nonempty_list(PAR, proc) { build_node $loc (Ast.Par($1)) }
;

proc:

;

nondetermlist:
    separated_nonempty_list(PLUS, seq) { build_node $loc (Ast.NonDeterm($1)) }
;

seq:
    ID { build_node $loc (Ast.Discard($1)) }
;