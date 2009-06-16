open Common
open Board

exception Ko

type t = (Board.t * move * hasht) list

(* internal state variables *)
      
let size = ref B9

let size_int () = int_of_boardsize !size

let log = open_out "log-moves.txt"
    
let h = ref []
    
let last_board = ref (Board.empty !size)
    
let last_move = ref None
    
let next_turn = ref `Black
    
let whitecaptures = ref 0
    
let blackcaptures = ref 0
    
(* functions that work on above data *)
    
let set_boardsize bs = size := bs
    
let rec take n l =
  if n = 0 then []
  else match l with
    x::xs -> x::(take (n-1) xs)
  | [] -> []
	
let handicap_stones s = 
  let (high,mid,low) = 
    match s with 
      B9  -> (6, 4, 2)
    | B11 -> (8, 5, 2)
    | B13 -> (9, 6, 3)
    | B19 -> (15, 9, 3)
  in 
  List.map (fun p -> pos_of_pair (int_of_boardsize s) p)
    [(high,high); (low,low); (high,low); (low,high);
     (mid,low); (mid,high); (low,mid); (high,mid)]
    
let center_stone n = ((n-1)/2, (n-1)/2)
    
let init_history handicap bs =
  set_boardsize bs;
  let startboard = Board.empty bs in
  let do_moves = List.iter (fun i -> add_stone_mod startboard i `Black) in
  let size = (int_of_boardsize bs) in
  let add_handicap_stones n = 
    let stones = take n (handicap_stones bs) in
    do_moves stones
  and add_center_stone () =
    do_moves [pos_of_pair size (center_stone size)]
  in
  
  begin
    match handicap with
      None -> next_turn := `Black
    | Some n when (n mod 2) = 1 && n > 4 ->
	add_center_stone ();
	add_handicap_stones (n-1);
	next_turn := `White
    | Some n ->
	add_handicap_stones n;
	next_turn := `White
  end;
  
  (* startboard now has all the appropriate handicap stones *)
  last_board := startboard;
  h := [ (startboard, Pass `White, hash startboard) ];
  whitecaptures := 0; blackcaptures := 0

let to_gmp move = 
  match move with
    Move (p,color) -> (color, 1 + int_of_pos (size_int ()) p)
  | Pass color -> (color, 0)
	
let to_move (color, n) =
  let size = size_int () in
  match n with
    0 -> Pass color
  | k when k < size * size + 1 ->
      let row = (k-1) / size
      and col = (k-1) mod size in
      Move (pos_of_pair size (row, col), color)
  | _ ->
      raise (Invalid_argument ("Gmp move is outwith board - " ^ (string_of_int n)))
	
let record_move move =
  let string_of_color c = 
    match c with 
      `Black -> "B"
    | `White -> "W" in
  let s = match move with
    Move (p,color) ->
      let row,col = unsafe_pair_of_pos p in
      (string_of_int row) ^ " " ^
      (string_of_int col) ^ " " ^
      (string_of_color color)
  | Pass c ->
      "Pass" ^  (string_of_color c) in
  output_string log (s ^ "\n");
  flush log

let add board move =
  record_move move;
  h := (board,move,Board.hash board) :: !h;
  last_board := board;
  last_move := Some move


let found_ko board =
  let target = Board.hash board in
  List.exists 
    (fun hist -> match hist with
      (b,_,bh) when bh = target -> Board.equal b board
    | _ -> false)
    !h
    

let twopass () =
  match !h with
    [] | _::[] -> false
  | (_, Pass c1, _)::(_, Pass c2, _)::_ when c1 != c2 ->
      true
  | _ -> false
	
let make_move inter color =
  let (board', captures) = Board.make_move !last_board inter color in
  if found_ko board' then raise Ko
  else begin
    add board' (Move (inter,color));
    match color with 
      `White -> 
	next_turn := `Black;
	whitecaptures := !whitecaptures + captures
    | `Black ->
	next_turn := `White;
	blackcaptures := !blackcaptures + captures
  end

let make_turn inter =
  make_move inter !next_turn

let make_pass () =
  let color = !next_turn in
  next_turn := opposite_color color;
  match !h with
  | (b, _, _)::_ -> add b (Pass color)
  | [] -> assert false

let end_info () = (!last_board, (!whitecaptures, !blackcaptures))
