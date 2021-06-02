type ('a, 'b) t = ('a * 'b) list

let empty = []

let singleton x y = [(x, y)]

let mem x map = List.mem_assoc x map

let remove x map = List.filter (fun (x',_) -> x' <> x) map

let add x y map = remove x map @ [(x,y)] 

let find x map = List.assoc x map

let rec merg f map1 map2 =
    match map1 with
    | (x1,y1)::rest1 -> 
        if mem x1 map2 then (x1, f x1 (Some(y1)) (Some(find x1 map2))) :: merg f rest1 (remove x1 map2)
        else (x1, f x1 (Some(y1)) None) :: merg f rest1 map2
    | [] -> 
        match map2 with
        | (x2,y2)::rest2 -> (x2, f x2 None (Some(y2))) :: merg f [] rest2 
        | [] -> []
let merge f map1 map2 = 
    List.fold_left (fun map (x,y) -> match y with None -> map | Some(y) -> map @ [(x,y)]) [] (merg f map1 map2) 

let fold_left f z0 map = List.fold_left (fun z (x,y) -> f z x y) z0 map 

let fold_right f map z0 = List.fold_right (fun (x,y) z -> f x y z) map z0

let partition f map = List.partition (fun (x,y) -> f x y) map 
    
let map f map = List.map (fun (x,y) -> (x, f y)) map

let mapi f map = List.map (fun (x,y) -> (x, f x y)) map

let add_list xys map = List.fold_left (fun m (x,y) -> add x y m) map xys

let add_list2 xs ys map =
    if List.length xs <> List.length ys then raise (Invalid_argument "M.add_list2");
    List.fold_left2 (fun m x y -> add x y m) map xs ys

let remove_list xs map = List.fold_left (fun m x -> remove x m) map xs

let of_list ls = add_list ls empty

let to_list map = map

let of_list2 xs ys = 
    if List.length xs <> List.length ys then raise (Invalid_argument "M.of_list2");
    add_list2 xs ys empty