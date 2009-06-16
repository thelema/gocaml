val init_gui : int -> Common.board_size -> unit

val draw_board : unit -> unit

val ask_for_move : unit -> Common.action

val draw_end_board : Board.t -> (int * int) -> unit
