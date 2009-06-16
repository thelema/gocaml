open Common
open Graphics

let bg_color = rgb 128 255 0

let windowsize = ref 800
let boardsize = ref B9

(* Number of pixels between intersects *)
let grid_size = ref 0

let init_gui wsize bsize =
  windowsize := wsize; boardsize := bsize;
  grid_size := (!windowsize - 20) / (int_of_boardsize bsize);
  let s = " " ^ (string_of_int wsize) ^ "x" ^ (string_of_int wsize) ^ "+10+10" in
  open_graph s

(* We leave one grid space left and below the board *)
let closest_intersect x y =
  let g = !grid_size in
  let x' = (x - (g / 2)) / g in
  let y' = (y - (g / 2)) / g in 
(* adjust for upside-down y values with respect to col *)
  let row = ((int_of_boardsize !boardsize) - 1) - y' in 
  let col = x' in
  pos_of_pair (int_of_boardsize !boardsize) (row,col)
    
let rec ask_for_move () =
  let status = wait_next_event [Button_down; Key_pressed] in
  if status.keypressed then 
    match status.key with
      'p' | 'P' -> PassMove
    | 'q' | 'Q' -> Quit
    | x -> 
	Printf.printf "Unexpected keypress: %c\n" x; 
	ask_for_move ()
  else
    try 
      Click (closest_intersect status.mouse_x status.mouse_y)
    with Failure _ -> 
      Printf.printf "Position %d, %d is not on board\n" status.mouse_x status.mouse_y;
      ask_for_move ()
	
let coord_of_move p =
  let row,col = unsafe_pair_of_pos p in
  let y = ((int_of_boardsize !boardsize) - row) * !grid_size in
  let x = col * !grid_size + !grid_size in
  (x,y)

(*
let draw_point (x,y) gv =
  match gv with
    Empty -> ()
  | Stone c -> 
      set_stone_color c;
      draw_circle x y 2

let draw_stone_plus (x,y) c dead frcd = 
  let r = if dead then !grid_size / 4 else !grid_size / 2 in
  (match c with
    Black -> set_color black
  | White -> set_color white);
  fill_ellipse x y r r;
  if frcd then begin
    set_color blue;
    fill_ellipse x y 3 3
  end
*)

let draw_pos (x,y) gv o frcd = 
  let circle c r = 
    set_color c;
    fill_ellipse x y r r
  in
  (match gv, o with
    | `Empty, `Empty -> circle bg_color 2
    | `Empty, `Black -> circle black 2
    | `Empty, `White -> circle white 2
    | _, `Empty -> assert false
    | `Black, `Black -> circle black (!grid_size / 2)
    | `Black, `White -> circle black (!grid_size / 4)
    | `White, `White -> circle white (!grid_size / 2)
    | `White, `Black -> circle white (!grid_size / 4)
    );
  if frcd then circle blue 3

let draw_alive_stone p c = 
  let (x,y) = coord_of_move p in
  draw_pos (x,y) c c false

let draw_grid () =
  let g = !grid_size 
  and bs = int_of_boardsize !boardsize in
  set_color bg_color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color black;
  for y = 1 to bs do
    Graphics.moveto g (y*g);
    Graphics.rlineto ((bs-1) * g ) 0    
  done;
  for x = 1 to bs do
    Graphics.moveto (x*g) g;
    Graphics.rlineto 0 ((bs-1) * g)
  done
    
let inter_to_string x y = "row " ^ (string_of_int x) ^ ", col " ^ (string_of_int y) 

let move_to_string = function
    Move (p, `Black) ->
      "Black played at " ^ (sprint_pos p)
  | Move (p, `White) -> 
      "White played at " ^ (sprint_pos p)
  | Pass _ -> "Passed"

let highlight_stone move =
  set_color red;
  match move with
    Pass _ -> ()
  | Move (p,_) ->
      let (x,y) = coord_of_move p in
      fill_ellipse x y 2 2

let draw_status str =
  moveto !grid_size 1;
  set_color black;
  draw_string (str)

let drawboard_add board moveadd =
  display_mode false;
  draw_grid ();
  Board.iterstones draw_alive_stone board;
  moveadd (); (* this highlights the last stone played and prints the text *)
  synchronize ();
  display_mode true    

let draw_board_ext board moveopt = 
  let moveadd = match moveopt with
    None -> fun () -> ()
  | Some m -> fun () -> 
      draw_status (move_to_string m);
      highlight_stone m
  in
  drawboard_add board moveadd

let draw_board () = draw_board_ext !History.last_board !History.last_move

let vis_annotate p gv g = 
  let (x,y) = coord_of_move p 
  and o = Board.Group.owner g
  and f = Board.Group.is_forced g in
  draw_pos (x,y) gv o f

let showscore eboard (b,w) = 
  moveto !grid_size 1;
  set_color black;
  draw_string (Printf.sprintf "B: %i W: %i.5" b w)

let draw_end_board eboard points =
  display_mode false;
  draw_grid ();
  let (white, black) = Board.score eboard points in
  showscore eboard (black, white);  
  Board.iterij2 vis_annotate eboard;
  synchronize ();
  display_mode true

