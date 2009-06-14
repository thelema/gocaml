open Rawgmp;;


let rec rmt_move c history =
  try 
    let move = Gmp.receive_move c in
    let new_history = History.make_move history move
    in Gmp.send_ok c; 
    new_history
  with 
    Gmp.UnexpectedCommand cmd ->
      Rawgmp.add_to_log ("Expecting MoveCmd, but got " ^ command_to_string cmd ^ ".  Now waiting for move again");
      rmt_move c history
  | History.Ko ->
      Rawgmp.add_to_log "Denying last move due to Ko, now waiting for new remote move";
      Gmp.send_deny c;
      rmt_move c history
;;

let rec lcl_move c history =
  try
    let move = Gui.ask_for_move !Options.color in
    History.make_move history move
  with
    Board.Illegal_move _ 
  | History.Ko -> lcl_move c history
;;

(* Pipeline operator *)
let (++) x f = f x;;

let draw_board h = 
  let b = History.last_board h in
  let m = History.last_move h in
  Gui.draw_board b m;
  h
;;

let rec human_gameloop history =
  try
    let color = History.next_turn history in

    Gui.ask_for_move color 
    ++ History.make_move history 
    ++ draw_board
    ++ human_gameloop 
  with
    Board.Illegal_move _ 
  | History.Ko -> human_gameloop history
	;;

let xmit_gmp c h =
  (match (History.last_move h) with
    None -> ()
  | Some m -> Gmp.send_move c m);
  Gmp.wait_for_ok c true;
  h
;;

let rec gmp_gameloop c history =
  let color = History.next_turn history in
  if !Options.color = color 
  then (history ++ draw_board ++ lcl_move c ++ xmit_gmp c ++ gmp_gameloop c)
  else (history ++ draw_board ++ rmt_move c ++ gmp_gameloop c)
;; 

let passive_start gmp = 
  match Gmp.read_command gmp with
    NewGameCmd ->
      Gmp.send_ok gmp;
      Gmp.respond_to_queries gmp;
  | m -> failwith ("Expecting NewGameCmd, but got " ^ Rawgmp.command_to_string m ^ " instead.")
;;

let main () =
  Options.parse_command_line ();
  Gui.init_gui ();
  Gui.draw_board Board.empty None;
  if !Options.play_local then 
    human_gameloop History.empty
  else begin
    let gmp = Gmp.init_connection (Unix.open_process !Options.gmp_prog) in
    passive_start gmp;
    let state =	match !Options.handicap with
      None -> History.empty
    | Some n -> Handicap.setup n
    in gmp_gameloop gmp state
  end;
  0
;;

main();;
