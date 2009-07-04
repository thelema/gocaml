type color = [`Black | `White]

val opposite_color : color -> color
	
type game_val = [color | `Empty]

type board_size = B9 | B11 | B13 | B19

val int_of_boardsize : board_size -> int
val boardsize_of_int : int -> board_size

type 'a board
type board_pos

val board_size : 'a board -> int
val make_board : int -> 'a -> 'a board
val make_board_pos : int -> (board_pos -> 'a) -> 'a board

val clone_board : 'a board -> 'a board

val unsafe_pair_of_pos : board_pos -> int * int

val pos_of_pair : int -> int * int -> board_pos

module PosS : Set.S with type elt = board_pos

val poss_of_posl : board_pos list -> PosS.t

val sprint_pos : board_pos -> string
val print_pos : board_pos -> unit

val int_of_pos : int -> board_pos -> int

val index : 'a board -> board_pos -> 'a

val matrix_set : 'a board -> board_pos -> 'a -> unit

val enum : 'a board -> (board_pos * 'a) Enum.t

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
