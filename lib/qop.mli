val qop_h : float list -> int -> float list

val qop_x : float list -> int -> float list

(*val qop_y : 'a -> int -> 'a

val qop_z : 'a -> int -> 'a *)

val qop_cx : float list -> int -> int -> float list

val measure : float list -> int list -> (float list * int * float) list
