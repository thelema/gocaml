exception Ko;;

type t = (Board.t * Board.move) list;;

let empty = [];;

let log = open_out "log-moves.txt";;

let record_move move =
  let string_of_color c = 
    match c with 
	Board.Black -> "B"
      |	Board.White -> "W" in
  let s = match move with
    Board.Move (row,col,color) ->
      (string_of_int row) ^ " " ^
      (string_of_int col) ^ " " ^
      (string_of_color color)
  | Board.Pass c ->
      "Pass" ^  (string_of_color c) in
  output_string log (s ^ "\n");
  flush log
;;

let add history board move =
  record_move move;
  (board,move) :: history
;;

let last_board history =
  match history with
    [] -> Board.empty
  | (board,move)::xs -> board
;;

let last_move history =
  match history with
    [] -> None
  | (board,move)::xs -> (Some move)
;;

let found_ko history board =
  List.exists 
    (fun (b,m) -> 
      Board.equal b board)
    history
;;


let make_move history move =
  let board = last_board history in
  match move with
    Board.Pass _ ->
      add history board move
  | Board.Move _ ->
      let board' = 
	Board.make_move board move in
      if found_ko history board' 
      then raise Ko;
      add history board' move
;;
  
let other color = 
  match color with
    Board.Black -> Board.White
  | Board.White -> Board.Black
;;
  
let next_turn h =
  let move = last_move h in
  let last_color = match move with
    Some(Board.Move (_,_,c)) -> c
  | Some(Board.Pass c) -> c
  | None -> Board.White in
  (other last_color)
;;
