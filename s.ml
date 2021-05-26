(* Set-like List *)

(* Setとの違いは順番を保存すること 
union [1;5] [4;2] = [1;5;4;2] 
類似点はeltの重複がないこと *)
type 'a t = 'a list

let empty = []
let mem x xs = List.mem x xs
let singleton x = [x]
(* 順序を保つように注意して *)
let add x xs = if mem x xs then xs else xs@[x]
(* let add x ls = (List.filter (fun y -> y<>x) ls)@[x] *)
(* 作るときは遅いように見せて、参照するときは早く *)
let union xs1 xs2 = List.fold_left (fun xs y -> add y xs) xs1 xs2
let remove x xs = List.filter ((<>) x) xs
let diff xs1 xs2 = List.fold_left (fun xs y -> remove y xs) xs1 xs2

let of_list xs = List.fold_left (fun ls x -> add x ls) empty xs
(* fold_leftなのは、左からの順序を保つため *)
(* add x ls=union ls (singleton x) なのは、左からの順序を保つため *)
(* ex. x?(y1,y2) -> set=(y1,y2)    not set=(y2,y1) *)
(* of_list [1;2] = [1;2] *)

(* fold_rightとunion (singleton x) lsの組み合わせにすれば?????? *)
(* lsに重複があったとき、右側優先にするため ex.x?(y,y)->yは右側のyのことにしたい *)

(* 迷い *)
(* x?(y,y)で、of_list [y1;y2]は右側のy2を指すようにすべき？ *)

(* 仕様 of_list [1;2;3;2] = [1;2;3] *)

let to_list xs = xs

let fold_right f xs y = List.fold_right f xs y

let rec iter f = function
  | [] -> ()
  | x::xs -> f x; iter f xs



(* mapとの違い *)
(* 順番の保ち方、add参照 *)
