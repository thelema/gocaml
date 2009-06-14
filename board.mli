
(* Representation of the board *)

type t

type color = Black | White

type move = 
    Move of int * int * color
  | Pass of color

val empty : t
val board_to_string : t -> string

val make_move : t -> move -> t

val stones : t -> int

val hash : t -> int

val equal : t -> t -> bool

val to_list : t -> (int * int * color) list

(* White/Black score *)
val score : t -> (int * int)

exception Illegal_move of string * move

