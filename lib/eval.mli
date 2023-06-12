exception EvalException of Location.code_pos * string

type qstate = Complex.t list
type conf = Conf of qstate * Ast.program * float
type distr = Distribution of conf list

val eval : Ast.program -> int -> distr list
val string_of_conf: conf -> string
val string_of_distribution: distr -> string
val string_of_qstate: Complex.t list -> string
