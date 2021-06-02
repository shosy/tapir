type ('a, 'b) t
val empty : ('a, 'b) t
val singleton : 'a -> 'b -> ('a, 'b) t
val mem : 'a -> ('a, 'b) t -> bool
val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val merge : ('a -> 'b option -> 'c option -> 'd option) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
val find : 'a -> ('a, 'b) t -> 'b
val fold_left : ('a -> 'b -> 'c -> 'a) -> 'a -> ('b, 'c) t -> 'a
val fold_right : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val partition : ('a -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t * ('a, 'b) t
val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
val mapi : ('a -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val add_list : ('a * 'b) list -> ('a, 'b) t -> ('a, 'b) t 
val add_list2 : 'a list -> 'b list -> ('a, 'b) t -> ('a, 'b) t 
val remove_list : 'a list -> ('a, 'b) t -> ('a, 'b) t
val of_list : ('a * 'b) list -> ('a, 'b) t
val to_list : ('a, 'b) t -> ('a * 'b) list
val of_list2 : 'a list -> 'b list -> ('a, 'b) t
