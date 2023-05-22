exception Syntax_error of Location.lexeme_pos * string

(** Parse source code written in lqccs *)
val parse : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.program