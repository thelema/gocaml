open Common

exception Ko

val init_history : int -> board_size -> unit

val to_gmp : move -> (color * int)

val to_move : (color * int) -> move

val make_move :
    board_pos -> color -> unit
	
val make_turn :
    board_pos -> unit

val make_pass :
    unit -> unit

val add : 
    Board.t -> move -> unit

val next_turn : 
    color ref

val last_board :
    Board.t ref

val last_move :
    move option ref

val found_ko : 
    Board.t -> bool

val twopass :
    unit -> bool

val end_info : unit -> (Board.t * (int * int))

