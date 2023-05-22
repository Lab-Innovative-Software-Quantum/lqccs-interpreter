{
	
	open Parser

	exception Lexing_error of Location.lexeme_pos * string

	let raise_error lexbuf msg = 
		raise (Lexing_error((Location.to_lexeme_position lexbuf), msg))

	let create_hashtable size init =
		let tbl = Hashtbl.create size in
		List.iter (fun (key, value) -> Hashtbl.add tbl key value) init;
		tbl

	let keywords_table =
		create_hashtable 5 [
			("if", 		IF);
			("then", 	THEN);
			("else", 	ELSE);
			("true", 	BOOLEAN(true));
			("false", BOOLEAN(false));
		]

}

let newline = '\n' | '\r' '\n'
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let hexadecimal = (['a'-'f' 'A'-'F'] | digit) 
let integer = digit+ | "0x" hexadecimal+
let identifier = (letter | '_') (letter | digit | '_')*

(* Scanner specification *)
rule next_token = parse
	  [' ' '\t']+		{ next_token lexbuf }	(* ignore and skip whitespace *)
	| newline			{ Lexing.new_line lexbuf; next_token lexbuf }
	| integer as lit	
	{ 
		try (* int_of_string function recognizes hexadecimal notation too *)
			INTEGER(int_of_string lit) 
		with Failure _ -> 
			raise_error lexbuf "Not a valid integer or exceeds the range of integers representable in type int"
	}
	| identifier as word 
	{	(* identifier or keyword *)
		match Hashtbl.find_opt keywords_table word with 
		| Some token	-> token 
		| None			-> ID(word)
	}
	| "/*"		{ multilinecomment lexbuf }
	| "//"		{ singlelinecomment lexbuf }
	| "||"		{ PAR }
	| "+"		{ PLUS }
	| '('       { LPAREN }
	| ')'       { RPAREN }
	| "="       { EQ }
	| '>'       { GT }
	| '<'       { LT }
	| ">="      { GEQ }
	| "<="      { LEQ }
	| "!="      { NEQ }
	(*
	| '-'		{ SUB }
	| '*'		{ MULT }
	| '/'       { DIV }
	| '%'       { MOD }*)
	(*
	| "&&"      { AND }
	| "||"      { OR }
	| "!"      	{ NOT }
	| '['       { LBRACKET }
	| ']'       { RBRACKET }
	| '{'       { LBRACE }
	| '}'       { RBRACE }
	| ';'       { SEMICOL }*)
	| eof		{ EOF }
	| _			{ raise_error lexbuf  "Unexpected character" }