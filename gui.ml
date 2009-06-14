open Graphics;;

(* Number of pixels between intersects *)
let grid_size () = (!Options.winsize - 20) / !Options.board_size;;

(* We leave one grid space left and below the board *)
let closest_intersect x y =
  let g = grid_size () in
  let x' = x + (g / 2) in
  let y' = y + (g / 2) in 
  let row = (!Options.board_size - 1) - ((y' - g ) / g) in
  let col = (x' - g) / g in
  (row,col)
;;

let rec ask_for_move color =
  let within_board row col =
    row < !Options.board_size &&
    col < !Options.board_size in
  let status = wait_next_event [Button_down; Key_pressed] in
  if status.keypressed then 
    Board.Pass color
  else
    begin
      let (row,col) = closest_intersect status.mouse_x status.mouse_y in
      if within_board row col 
      then Board.Move (row,col,color)
      else ask_for_move color
    end
;;


let coord_of_move row col =
  let g = grid_size () in
  let y = ((!Options.board_size-1) - row) * g + g in
  let x = col * g + g in
  (x,y)
;;

let draw_stone row col color =
  let rgb = match color with
    Board.Black -> black
  | Board.White -> white in
  set_color rgb;
  let (x,y) = coord_of_move row col in
  let radius = (grid_size ()) / 2 in
  fill_ellipse x y radius radius
;;


let draw_grid () =
  let g = grid_size () in 
  set_color (rgb 128 255 0);
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color black;
  for y = 1 to !Options.board_size do
    Graphics.moveto g (y*g);
    Graphics.rlineto ((!Options.board_size-1) * g ) 0    
  done;
  for x = 1 to !Options.board_size do
    Graphics.moveto (x*g) g;
    Graphics.rlineto 0 ((!Options.board_size-1) * g)
  done;
;;

let move_to_string move =
  match move with 
    Board.Move (row,col,color) ->
      let name = match color with
	Board.Black -> "Black "
      | Board.White -> "White " in 
      name ^ "played at row " ^ (string_of_int row) ^ ", col " ^ (string_of_int col)
  | Board.Pass color -> "Passed"
;;

let highlight_stone move =
  set_color red;
  match move with
    Board.Pass _ -> ()
  | Board.Move (r,c,_) ->
      let (x,y) = coord_of_move r c in
      fill_ellipse x y 2 2
;;

let draw_board board move =
  display_mode false;
  draw_grid ();
  List.iter 
    (fun (row,col,color) -> draw_stone row col color)
    (Board.to_list board);
  moveto (grid_size ()) 1;
  set_color black;
  begin match move with
    Some m -> 
      begin
	draw_string (move_to_string m);
	highlight_stone m
      end
  | None -> ()
  end;
  synchronize ();
  display_mode true
;;

let init_gui () =
  let height = !Options.winsize in
  let width = !Options.winsize in
  let s = " " ^ (string_of_int height) ^ "x" ^ (string_of_int width) ^ "+10+10" in
  open_graph s;
;;

