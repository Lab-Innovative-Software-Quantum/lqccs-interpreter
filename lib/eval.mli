exception EvalException of Location.code_pos * string

val eval : Ast.program -> unit
