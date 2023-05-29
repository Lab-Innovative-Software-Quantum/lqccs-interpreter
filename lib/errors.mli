(** Prints an error of a single line in the source code *)
val report_singleline: string -> string -> Location.lexeme_pos -> string -> unit

(** Prints an error of multiple lines long in the source code *)
val report_multiline: string -> string -> Location.code_pos -> string -> unit