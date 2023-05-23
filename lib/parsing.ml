exception Syntax_error of Location.lexeme_pos * string

let parse scanner lexbuf =
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error _ -> 
    let position = Location.to_lexeme_position lexbuf in
    raise(Syntax_error(position, "Parsing error"))
