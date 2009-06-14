open Rawgmp;;

exception GmpError of string * Rawgmp.raw_gmp_message;;
exception UnexpectedCommand of Rawgmp.gmp_command;;

type gmp_connection = 
    { mutable last_hsn : int;
      mutable last_ysn : int;
      mutable buffer : gmp_command option;
      raw_conn : Rawgmp.raw_gmp_connection
    } ;;

let init_connection inout =
  { last_hsn = 0;
    last_ysn = 0;
    buffer = None;
    raw_conn = Rawgmp.init_connection inout}
;;

(* A new msg has a new seqn no. *)
let is_new_cmd c msg = 
  match msg.command with
    OkCmd -> true
  | _ -> c.last_hsn != msg.ysn
;;

(* Should never stash Ok's *)
let stash_command c msg =
  c.last_hsn <- msg.ysn;
  if msg.command = OkCmd
  then c.buffer <- None
  else c.buffer <- Some msg.command;

  if c.buffer != None 
  then Rawgmp.add_to_log
      "Processed implicit Ok";
;;

let rec wait_for_ok c allow_implicit =
  try
    let msg = Rawgmp.read_message c.raw_conn in
    match msg.command with 
      OkCmd -> ()
    | _ ->
	if (is_new_cmd c msg) then
	  if allow_implicit 
	  then stash_command c msg
	  else raise (GmpError ("Waiting for explicit Ok, but got other command", msg))
	else raise (GmpError ("Rejected duplicate msg while waiting for Ok", msg))
	    
  with
    GmpError (s,msg) ->
      begin
	add_to_log s;
	wait_for_ok c allow_implicit
      end
;;

let rec read_command c =
  match c.buffer with 
    Some x -> 
      c.buffer <- None; x;
  | None ->
      let msg = Rawgmp.read_message c.raw_conn in
      if (is_new_cmd c msg) then 
	begin
	  c.last_hsn <- msg.ysn;
	  msg.command;
	end
      else 
	begin
	  Rawgmp.add_to_log "Rejected command due to seqn error";
	  read_command c
	end
;;

let send_ok c =
  let msg = 
    { hsn = c.last_hsn;
      ysn = c.last_ysn;
      command = Rawgmp.OkCmd } in
  Rawgmp.send_message c.raw_conn msg;
;;

let send_deny c =
  let msg = 
    { hsn = c.last_hsn;
      ysn = c.last_ysn;
      command = Rawgmp.DenyCmd } in
  Rawgmp.send_message c.raw_conn msg;
;;

let send_answer c n =
  let msg = 
    { hsn = c.last_hsn;
      ysn = 1 - c.last_ysn;
      command = (Rawgmp.AnswerCmd n)}
  in
  Rawgmp.send_message c.raw_conn msg;
  c.last_ysn <- msg.ysn;
;;

let to_gmp_move move =
  match move with
    Board.Move (r,c,color) -> (color, 1 + !Options.board_size * r + c)
  | Board.Pass color -> (color, 0)
;;

let send_move c move = 
  let (col,n) = to_gmp_move move in
  let movecmd = Rawgmp.MoveCmd (col,n)
  in let msg = 
    { hsn = c.last_hsn;
      ysn = 1 - c.last_ysn;
      command = movecmd } in
  Rawgmp.send_message c.raw_conn msg;
  c.last_ysn <- msg.ysn;
;;

let receive_move gmp =
  let cmd = read_command gmp in

  let max_intersect = (!Options.board_size * !Options.board_size) + 1 in

  match cmd with
    MoveCmd (color,n) ->
      if n = 0 then 
	Board.Pass color
      else if n > max_intersect then 
	raise (Invalid_argument ("Gmp move is outwith board - " ^ (string_of_int n)))
      else 
	let row = (n-1) / !Options.board_size in
	let col = (n-1) mod !Options.board_size	in
	Board.Move (row,col,color)
  | _ -> raise (UnexpectedCommand cmd)
;;

let send_query c q =
  let msg = 
    { hsn = c.last_hsn;
      ysn = 1 - c.last_ysn;
      command = (Rawgmp.QueryCmd q)} in

  Rawgmp.send_message c.raw_conn msg;
  c.last_ysn <- msg.ysn
;;


let rec wait_for_answer c =
  try
    let msg = Rawgmp.read_message c.raw_conn in
    if (is_new_cmd c msg) then
      match msg.command with
	AnswerCmd n -> n
      | _ -> raise (GmpError ("Received unexpected command while waiting for Answer", msg))
    else raise (GmpError ("Out of sequence command received while waiting for Answer", msg))
  with
    GmpError (s,msg) ->
      begin
	Rawgmp.add_to_log s;
	wait_for_answer c
      end
;;

let perform_query c q =
  send_query c q;
  wait_for_answer c
;;

let send_newgame c =
  let msg = 
    { hsn = c.last_hsn;
      ysn = 1 - c.last_ysn;
      command = Rawgmp.NewGameCmd }
  in
  Rawgmp.send_message c.raw_conn msg;
  c.last_ysn <- msg.ysn;
;;

(* Handles incoming queries.  This assumes that we get all of the queries
   in one big block! *)
let rec respond_to_queries c =
  let respond_to conn q = 
    let answer = 
      match q with
	QueryGame -> 1 (* Go *)
      | QueryModemBufferSize -> Rawgmp.buffer_size
      | QueryProtocolVersion -> 0
      | QueryStonesOnBoard -> 0
      | QueryBlackTime -> 0
      | QueryWhiteTime -> 0
      | QueryCharacterSet -> 1
      | QueryRules -> 1
      | QueryHandicap -> 
	  begin
	    match !Options.handicap with
	      None -> 1
	    | Some n -> n 
	  end
      | QueryBoardSize -> !Options.board_size
      | QueryTimeLimit -> 60 (* min *)
      | QueryComputerPlayerColor -> 
	  begin
	    match !Options.color with
	      Board.White -> 1
	    | Board.Black -> 2
	  end
      | QueryWhoAreYou -> 0 in
    send_answer conn answer in

  let m = read_command c in
  match m with 
    (QueryCmd q) -> 
      begin
	respond_to c q;
	respond_to_queries c
      end
  | OkCmd -> ()
  | m -> failwith ("Unexpected command during queries: " ^ command_to_string m)
;;

