exception Syntax_error of Location.lexeme_pos * string

(** Parse source code written in microc *)
val parse : string -> (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.program