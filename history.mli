type t

val empty : t

exception Ko

val make_move :
    t -> Board.move -> t

val add : 
    t -> Board.t -> Board.move -> t

val next_turn : 
    t -> Board.color

val last_board :
    t -> Board.t

val last_move :
    t -> Board.move option

val found_ko : 
    t -> Board.t -> bool
