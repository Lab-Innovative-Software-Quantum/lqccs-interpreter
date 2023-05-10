(*
	Print the error to stderr.

	Error example:
	Syntax error, lines 1-6: Invalid definition of the 'main' function. The signature must be 'int main()' or 'void main()'
	int main(int a) {
		bool b;
		b = false;
		int a[];
*)


(** Prints an error of a single line in the source code *)
val report_singleline: string -> string -> Location.lexeme_pos -> string -> unit

(** Prints an error of multiple lines long in the source code *)
val report_multiline: string -> string -> Location.code_pos -> string -> unit