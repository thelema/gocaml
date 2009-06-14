val init_gui : int -> Global.board_size -> unit

val draw_board : unit -> unit

val ask_for_move : unit -> Global.action

val draw_end_board : Board.t -> (int * int) -> unit
