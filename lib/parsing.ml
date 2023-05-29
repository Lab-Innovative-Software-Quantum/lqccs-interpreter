exception Syntax_error of Location.lexeme_pos * string

let error_message state =
  try
    let msg = match ParserMessages.message state with
      "<YOUR SYNTAX ERROR MESSAGE HERE>\n" -> "Parsing error"
    | msg -> msg
  in
    Printf.sprintf "%s (state %d)" msg state
  with Not_found ->
    Printf.sprintf "Unknown syntax error (state %d)" state

let parse scanner lexbuf =
  try
    Parser.program scanner lexbuf
  with
  | Parser.Error state -> 
    let message = error_message state in
    let position = Location.to_lexeme_position lexbuf in
    raise(Syntax_error(position, message))
