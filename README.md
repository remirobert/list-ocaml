list-ocaml
==========

What is this
============
Record list module to learn Caml language

Function
=======

* length : ’a my_list -> int
* hd : ’a my_list -> ’a
* tl : ’a my_list -> ’a my_list
* nth : ’a my_list -> int -> ’a
* rev : ’a my_list -> ’a my_list
* append : ’a my_list -> ’a my_list -> ’a my_list
* rev_append : ’a my_list -> ’a my_list -> ’a my_list
* flatten : ’a my_list my_list -> ’a my_list
* iter : (’a -> ’b) -> ’a my_list -> unit
* map : (’a -> ’b) -> ’a my_list -> ’b my_list
* fold_left : (’a -> ’b -> ’a) -> ’a -> ’b my_list -> ’a
* for_all : (’a -> bool) -> ’a my_list -> bool
* exists : (’a -> bool) -> ’a my_list -> bool
* mem : ’a -> ’a my_list -> bool
* memq : ’a -> ’a my_list -> bool
* filter : (’a -> bool) -> ’a my_list -> ’a my_list
* mem_assoc : ’a -> (’a * ’b) my_list -> bool
* assoc : ’a -> (’a * ’b) my_list -> ’b
* split : (’a * ’b) my_list -> ’a my_list * ’b my_list
* remove_assoc : ’a -> (’a * ’b) my_list -> (’a * ’b) my_list
