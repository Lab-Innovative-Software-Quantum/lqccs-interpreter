exception TypeException of string * Location.code_pos

val typecheck : Ast.program -> Ast.program
