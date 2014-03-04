type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

let rec length = (fun (list:'a my_list) ->
  match list with
  | Empty -> 0
  | Item (data, next) -> length next + 1)

let rec display_content_list = (fun (list:'a my_list) ->
  match list with
  | Empty -> 0 
  | Item (data, next) -> print_string data;
			 print_endline "";
			 display_content_list next + 1)

let get_first_elem = (fun (list:'a my_list) ->
  match list with
  | Empty -> failwith "Empty list"
  | Item (data, next) -> data)

let rec get_last_elem = (fun (list:'a my_list) ->
  match list with
  | Empty -> failwith "Empty list"
  | Item (data, Empty) -> data
  | Item (data, next) -> get_last_elem next)

let tl  = (fun (list:'a my_list) ->
  match list with
  | Empty -> Empty 
  | Item (data, Empty) -> Empty
  | Item (data, next) -> next)

let rec nth  = (fun (list:'a my_list) (nb:int) ->
  match nb with
  | i when nb > length list -> failwith "The list is too short"
  | _ ->
     match list with
     | Empty -> failwith "Empty list"
     | Item (data, next) -> 
	if nb == 0 
	then data
	else nth next (nb - 1))

let rec append = (fun (list1:'a my_list) (list2:'a my_list) ->
  match list1 with
  | Empty -> list2
  | Item (data, next) -> Item (data, append next list2))

let rec rev = (fun (list:'a my_list) ->
  match list with
  | Empty -> Empty
  | Item (data, next) -> append (rev next) (Item (data, Empty)))

let rec rev_append = (fun (list1:'a my_list) (list2:'a my_list) ->
  let list = rev list1 in
  match list with
  | Empty -> list2
  | Item (data, next) -> Item (data, append next list2))

let rec exists = (fun (f:'a -> bool) (list:'a my_list) ->
  match list with
  | Empty -> false
  | Item (data, next) -> f data || exists f list)
    
let rec iter = (fun (f:'a -> 'b) (list:'a my_list) ->
  match list with
  | Empty -> ()
  | Item (data, next) -> f data; iter f next)

let rec map = (fun (f:'a -> 'b) (list:'a my_list) ->
  match list with
    | Empty -> Empty
    | Item (data, next) -> Item (f data, map f next))

let rec mem = (fun (elem:'a) (list:'a my_list) ->
  match list with 
  | Empty -> false
  | Item (data, next) -> 
    if compare data elem = 0
    then true
    else mem elem list)

let rec memq = (fun (elem:'a) (list:'a my_list) ->
  match list with 
  | Empty -> false
  | Item (data, next) -> 
    if data = elem
    then true
    else mem elem list)

let rec filter = (fun (f:'a -> bool) (list:'a my_list) ->
  match list with
  | Empty -> Empty
  | Item (data, next) -> 
    if f data = true
    then Item (data, filter f next)
    else filter f next)

let rec assoc = (fun (key:'a) (list:('a * 'b) my_list) ->
    match list with
    | Empty -> Empty
    | Item ((data1, data2), next) -> 
      if compare data1 key = 0
      then data2
      else assoc key next)

let rec mem_assoc = (fun (key:'a) (list:('a * 'b) my_list) ->
    match list with
    | Empty -> false
    | Item ((data1, data2), next) -> 
      if compare data1 key = 0
      then true
      else mem_assoc key next)

let rec remove_assoc = (fun (key:'a) (list:('a * 'b) my_list) ->
  match list with
  | Empty -> list
  | Item ((data1, data2), next) ->
    if compare key data1 = 0
    then Item ((data1, data2), next)
    else remove_assoc key next)

let rec for_all = (fun (f:('a -> bool)) (list:'a my_list) ->
    match list with
    | Empty -> false
    | Item (data, next) ->
      if f data = false
      then false
      else for_all f list)

let rec fold_left = (fun f (elem:'a) (list:'b my_list) ->
  match list with
  | Empty -> Empty
  | Item (data, next) -> fold_left f (f elem data) next)

let rec split = (fun (l:('a * 'b) my_list) ->
  match l with
  | Empty -> (Empty, Empty)
  | Item ((data1, data2), next) ->
    let (d1, d2) = split next in (d1, d2))

let l = Item ("001", Item ("002", Item ("003", Empty)));;
let l2 = Item ("110", Item ("112", Item ("113", Empty)));;

print_endline "try rev append";;

iter print_string l;;
