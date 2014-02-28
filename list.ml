type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty

let rec length (list:'a my_list) : int =
  match list with
  | Empty -> 0
  | Item (data, next) -> length next + 1

let rec display_content_list (list:'a my_list) : int =
  match list with
  | Empty -> 0 
  | Item (data, next) -> print_string data;
			 print_endline "";
			 display_content_list next + 1 

let get_first_elem (list:'a my_list) : 'a =
  match list with
  | Empty -> failwith "Empty list"
  | Item (data, next) -> data

let rec get_last_elem (list:'a my_list) : 'a = 
  match list with
  | Empty -> failwith "Empty list"
  | Item (data, Empty) -> data
  | Item (data, next) -> get_last_elem next

let tl (list:'a my_list) : 'a my_list = 
  match list with
  | Empty -> failwith "Empty list" 
  | Item (data, Empty) -> Empty
  | Item (data, next) -> next


let nth (l:'a my_list) (nb:int) : 'a = 
  match nb with
  | -1 -> failwith "Invalid Argument"
  | v when nb > length l -> failwith "the list is too short"
  | _ ->
     match l with
     | Empty -> failwith "Empty list"
     | Item (data, next) -> data

let l = Item ("salut", Item ("remi double", Item ("salut1", Empty)));;

print_endline "test my_list";;
let size = length l;;

display_content_list l;;

print_int size;;

print_endline "test tl"
let last = tl l;;
display_content_list last;;

let elem = nth;;
