type 'a t

val empty : 'a t
val mem : 'a -> 'a t -> bool
val singleton : 'a -> 'a t
val add : 'a -> 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
val union : 'a t -> 'a t -> 'a t
val diff : 'a t -> 'a t -> 'a t
val remove_list : 'a list -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : ('a -> 'unit) -> 'a t -> unit
