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
		create_hashtable 13 [
			("int", 	INT);
			("char", 	CHAR);
			("bool", 	BOOL);
			("void", 	VOID);
			("if", 		IF);
			("else", 	ELSE);
			("for", 	FOR);
			("while", 	WHILE);
			("return", 	RETURN);
			("true", 	BOOLEAN(true));
			("false", 	BOOLEAN(false));
			("do", 		DO);
			("NULL", 	NULL)
		]
	
	(* special characters are ', \b, \t, \, \r, and \n *)
	let to_special_character c = match c with
		  'b'	-> Some '\b'
		| 't'	-> Some '\t'
		| '\\'	-> Some '\\'
		| 'r'	-> Some '\r'
		| 'n'	-> Some '\n'
		| '\'' 	-> Some '\''
		| _ 	-> None
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
	| "'" 		{ readchar lexbuf }
	| "true"	{ BOOLEAN(true) }
	| "false"	{ BOOLEAN(false) }
	| "++"		{ INCREMENT }
	| "--"		{ DECREMENT }
	| "+="		{ SHORTADD }
	| "-="		{ SHORTSUB }
	| "*="		{ SHORTMULT }
	| "%="		{ SHORTMOD }
	| "/="		{ SHORTDIV }
	| '+'		{ ADD }
	| '-'		{ SUB }
	| '*'		{ MULT }
	| '/'       { DIV }
	| '%'       { MOD }
	| '='       { ASSIGN }
	| '>'       { GT }
	| '<'       { LT }
	| "=="      { EQ }
	| ">="      { GEQ }
	| "<="      { LEQ }
	| "!="      { NEQ }
	| "&&"      { AND }
	| "||"      { OR }
	| "!"      	{ NOT }
	| '('       { LPAREN }
	| ')'       { RPAREN }
	| '['       { LBRACKET }
	| ']'       { RBRACKET }
	| '{'       { LBRACE }
	| '}'       { RBRACE }
	| ';'       { SEMICOL }
	| ','       { COMMA }
	| '&'		{ AMPERSAND }
	| eof		{ EOF }
	| _			{ raise_error lexbuf  "Unexpected character" }

and multilinecomment = parse
	| "*/"			{ next_token lexbuf }
	| newline		{ Lexing.new_line lexbuf; multilinecomment lexbuf }
	| _				{ multilinecomment lexbuf }
	| eof			{ raise_error lexbuf "Multiline comment not closed" }

and singlelinecomment = parse
	| newline		{ Lexing.new_line lexbuf; next_token lexbuf }
	| _				{ singlelinecomment lexbuf }
	| eof			{ EOF }

and readchar = parse
	  "'"					{ raise_error lexbuf "Missing character between single quotes" }
	| '\\' (_ as c) '\''	{ 
								match to_special_character(c) with
									  Some ch 	-> CHARACTER(ch)
									| None 		-> raise_error lexbuf "Invalid special character"
							}
	| [^ '\''] as c '\''	{ CHARACTER(c) }
	| _						{ raise_error lexbuf "Character not terminated" }
