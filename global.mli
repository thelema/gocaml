type color = [`Black | `White]

val opposite_color : color -> color
	
type game_val = [color | `Empty]

type board_size = B9 | B11 | B13 | B19

val int_of_boardsize : board_size -> int
val boardsize_of_int : int -> board_size

type board_pos

val unsafe_pair_of_pos : board_pos -> int * int

val pos_of_pair : int -> int * int -> board_pos

module PosS : Set.S with type elt = board_pos

val poss_of_posl : board_pos list -> PosS.t

val sprint_pos : board_pos -> string
val print_pos : board_pos -> unit

val int_of_pos : int -> board_pos -> int

val index : 'a array array -> board_pos -> 'a

val matrix_set : 'a array array -> board_pos -> 'a -> unit

val walk : (board_pos -> 'a -> 'a) -> int -> 'a -> 'a

val nbrs : int -> board_pos -> board_pos list

val in_size : int -> board_pos -> bool

val int_of_pos : int -> board_pos -> int

type action = 
    Click of board_pos
  | PassMove
  | Quit

type move = 
    Move of board_pos * color
  | Pass of color

val uniq : 'a list -> 'a list (* removes duplicates from a list *)

val (==>) : 'a -> ('a -> 'b) -> 'b
