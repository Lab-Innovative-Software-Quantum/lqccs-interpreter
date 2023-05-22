open Ast
exception Syntax_error of Location.lexeme_pos * string

let parse filename scanner lexbuf =
  Lexing.set_filename lexbuf filename;
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error -> 
    let position = Location.to_lexeme_position lexbuf in
    raise(Syntax_error(position, "Parsing error"))
