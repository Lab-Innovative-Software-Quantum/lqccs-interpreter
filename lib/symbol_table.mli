exception DuplicateEntry of Ast.var

type 'a t

val empty_table : 'a t
val begin_block : 'a t -> 'a t
val end_block : 'a t -> 'a t
val add_entry : Ast.var -> 'a -> 'a t -> 'a t
val remove_entry : Ast.var -> 'a t -> 'a t
val lookup : Ast.var -> 'a t -> 'a option
val of_alist : (Ast.var * 'a) list -> 'a t
