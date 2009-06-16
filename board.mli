open Common

module Group : sig
  type t
  val is_dead : t -> bool
  val is_forced : t -> bool
  val owner : t -> game_val
end

(* Representation of the board *)
type t 
      
val empty : board_size -> t
    
val board_to_string : t -> string
    
val add_stone_mod : t -> board_pos -> color -> unit

exception Illegal_move of string * board_pos
	    
val make_move : t -> board_pos -> color -> (t * int) (* throws Illegal_move*)

val stones : t -> int
    
type hasht (* internally just int *)
      
val hash : t -> hasht
    
val equal : t -> t -> bool
    
val on_board : t -> board_pos -> bool
    
val iterstones : (board_pos -> color -> unit) -> t -> unit 
    
(*val iterij : (int -> int -> 'a -> unit) -> 'a array array -> unit*)

val iterij2 : (board_pos -> game_val -> Group.t -> unit) -> t -> unit

(*val is_dead_stone : stone_props -> bool*)

(* White,Black score *)
val score : t -> (int * int) -> (int * int)
    
val forcetoggle : t -> board_pos -> unit
