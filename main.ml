open Common
(*open Rawgmp *)

exception User_exit

type player_type = Local | Remote of string

let gmp_prog = "gnugo --mode=gmp --quiet -o log-sgf.txt --score all 2>log-gnugo.txt"
let handicap = ref None
let color : color ref = ref `Black
let white_player = ref (Remote gmp_prog)
let black_player = ref Local
let board_size = ref B9;;
let winsize = ref 700;;

let options = 
  [("-handicap", 
    Arg.Int (fun n -> 
      if n < 2 then raise (Arg.Bad "Handicap must be 2 or greater")
      else handicap := (Some n)),
    "Sets blacks advantage");

   ("-color",
    Arg.String (fun s -> match s with
      "white" -> color := `White
    | "black" -> color := `Black
    | _ -> raise (Arg.Bad "Color must be 'black' or 'white'")), 
    "Chooses local color");

   ("-local",
    Arg.Unit (fun () -> white_player := Local; black_player := Local),
    "Run gocaml without a remote player");

   ("-boardsize",
    Arg.Int (fun n -> match n with
      9 | 11 | 13 | 19 -> board_size := boardsize_of_int n
    | _ -> raise (Arg.Bad "Board size must be 9, 11, 13 or 19")),
    "Sets the board size");

   ("-winsize", Arg.Int (fun n -> winsize := n), 
    "Sets window dimensions");

   ("-gmpprog", Arg.String (fun s -> white_player := Remote s), 
    ("Sets GMP-compatible program to use (default is " ^ gmp_prog));
];;

let errmsg = "Gocaml v1.0.\n\nCommand line options are:";;

let parse_command_line () =
  Arg.parse options ignore errmsg

let rec rmt_move c =
  try 
    begin
      match Gmp.receive_move c with
	Move (p,_) -> History.make_turn p
      | Pass _ -> History.make_pass ();
    end;
    Gmp.send_ok c; 
  with 
    Gmp.UnexpectedCommand cmd ->
      Rawgmp.add_to_log ("Expecting MoveCmd, but got " ^ Rawgmp.command_to_string cmd ^ ".  Now waiting for move again");
      rmt_move c 
  | History.Ko ->
      Rawgmp.add_to_log "Denying last move due to Ko, now waiting for new remote move";
      Gmp.send_deny c;
      rmt_move c 

let rec lcl_move () =
  try
    match Gui.ask_for_move () with 
      Click p -> History.make_turn p
    | PassMove -> History.make_pass ()
    | Quit -> raise User_exit
  with
    Board.Illegal_move (n, p) -> Printf.printf "Illegal: %s \n" n; lcl_move ()
  | History.Ko -> print_string "Ko!\n"; lcl_move ()

let xmit_gmp c =
  (match !History.last_move with
    None -> ()
  | Some m -> Gmp.send_move c m);
  Gmp.wait_for_ok c true

let gameloop (wi, wo) (bi, bo) =
(*
  let bi () = Printf.printf "<BI>"; bi (); Printf.printf "</BI>\n"
  and bo () = Printf.printf "<BO>"; bo (); Printf.printf "</BO>\n"  
  and wi () = Printf.printf "<WI>"; wi (); Printf.printf "</WI>\n"
  and wo () = Printf.printf "<WO>"; wo (); Printf.printf "</WO>\n"
  in
*)
  try 
    let rec inner_loop () =
      if History.twopass () then History.end_info ()
      else begin
	Gui.draw_board ();
	match !History.next_turn with
	  `Black -> bi (); wo (); inner_loop ()
	| `White -> wi (); bo (); inner_loop ()
      end
    in
    inner_loop ()
  with 
    User_exit -> History.end_info ()

let passive_start gmp = 
  match Gmp.read_command gmp with
    Rawgmp.NewGameCmd ->
      Gmp.send_ok gmp;
      Gmp.respond_to_queries gmp;
  | m -> failwith ("Expecting NewGameCmd, but got " ^ Rawgmp.command_to_string m ^ " instead.")

let rec score_loop (b, captures) =
  Gui.draw_end_board b captures;
  match Gui.ask_for_move () with
  | PassMove | Quit -> ()
  | Click p -> 
      Board.forcetoggle b p;
      score_loop (b, captures)
	
let main () =
  parse_command_line ();
  let iofuncs = function 
      Local -> (lcl_move , (fun () -> ()))
    | Remote s -> 
	let gmp = Gmp.init_connection (Unix.open_process s) in
	passive_start gmp;
	((fun () -> rmt_move gmp), (fun () -> xmit_gmp gmp)) 
  in
  Gui.init_gui !winsize !board_size;
  History.init_history !handicap !board_size;
  Gui.draw_board ();
  gameloop (iofuncs !white_player) (iofuncs !black_player) |>
    score_loop
    
let _ = main ();;
