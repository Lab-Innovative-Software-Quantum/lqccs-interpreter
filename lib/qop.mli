open Complex

module Mat = Owl.Dense.Matrix.Z

exception QuantumException of string

type measurement_result = {quantum_state: t list; value: int list; probability: float}

(* Quantum operations *)
val qop_h : t list -> int -> t list

val qop_x : t list -> int -> t list

val qop_z : t list -> int -> t list

val qop_y : t list -> int -> t list

val qop_cx : t list -> int -> int -> t list

val qop_i : t list -> int -> t list

val measure : t list -> int list -> (t list * int * float) list 