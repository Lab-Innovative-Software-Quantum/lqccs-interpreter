open Owl

type measurement_result = {quantum_state: float list; value: int list; probability: float}

(* Utility functions *)
val make_id : Mat.mat

val list2vec : float list -> Mat.mat

val vec2list : Mat.mat -> float list 

val step_forwards : Mat.mat -> int -> Mat.mat

val step_backwards : Mat.mat -> int -> Mat.mat

val generate_cases : int -> int list -> int list list

(* Quantum operations *)
val qop_h : float list -> int -> float list

val qop_x : float list -> int -> float list

val qop_z : float list -> int -> float list

val qop_cx : float list -> int -> int -> float list

val qop_i : float list -> int -> float list

val measure : float list -> int list -> (float list * int * float) list 