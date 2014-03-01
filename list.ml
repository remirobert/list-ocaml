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

let rec nth (list:'a my_list) (nb:int) : 'a = 
  match nb with
  | i when nb > length list -> failwith "The list is too short"
  | _ ->
     match list with
     | Empty -> failwith "Empty list"
     | Item (data, next) -> 
	if nb == 0 
	then data
	else nth next (nb - 1)

let rec append (list1:'a my_list) (list2:'a my_list) : 'a my_list = 
  match list1 with
  | Empty -> list2
  | Item (data, next) -> Item (data, append next list2)

let rec rev (list:'a my_list) : 'a my_list = 
  match list with
  | Empty -> Empty
  | Item (data, next) -> append (rev next) (Item (data, Empty))

let l = Item ("salut", Item ("remi double", Item ("salut1", Empty)));;
let l2 = Item ("toto", Item ("123", Item ("ok ca marche", Empty)));;

let append_list = append l l2;;
let size = length l;;
let size1 = length l2;;
let sizea = length append_list;;
rev append_list;;
display_content_list l;;
let rev_list = rev l;;
display_content_list rev_list;;

