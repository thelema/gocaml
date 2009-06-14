open Global
(* In log, lsn and rsn are local seqn and remote seqn.
*)

let buffer_size = 256;;

let log = open_out "log-gmp.txt";;

let _ = 
  output_string log 
    ("GMP message log\n\n" ^ 
     "Note: 'lsn' is local sequence number and 'rsn' is remote sequence number.  This\n" ^
     "avoids the role-reversal headaches of the GMP hsn and ysn.\n\n");

exception GmpParseFailed;;

type gmp_query =
    QueryGame
  | QueryModemBufferSize
  | QueryProtocolVersion
  | QueryStonesOnBoard
  | QueryBlackTime
  | QueryWhiteTime
  | QueryCharacterSet
  | QueryRules
  | QueryHandicap
  | QueryBoardSize
  | QueryTimeLimit
  | QueryComputerPlayerColor
  | QueryWhoAreYou
;;

type gmp_command = 
    OkCmd | 
    DenyCmd | 
    NewGameCmd |
    QueryExtendedCmd of int |
    QueryCmd of gmp_query |
    AnswerCmd of int |
    MoveCmd of color * int |
    TakebackMoveCmd of int
;;

type raw_gmp_message = 
    { hsn : int;
      ysn : int;
      command : gmp_command
    } 
;;

(* Message parsing functions *)

let his_seqn_no msg =
  let b = int_of_char 
      (String.get msg 0) 
  in (b land 2) lsr 1
;;

let your_seqn_no msg =
  let b = int_of_char 
      (String.get msg 0)
  in (b land 1)
;;

let extract_raw_cmd msg =
  let b = int_of_char 
      (String.get msg 2) in
  (b land 0x70) lsr 4
;;

let extract_raw_value msg =
  let b1 = int_of_char 
    (String.get msg 2) in
  let b2 = int_of_char 
      (String.get msg 3) in
  let high = (b1 land 0x7) lsl 7 in
  let low = b2 land 0x7f in
  (high lor low)
;;

let extract_query value =
  match value with 
    0 -> QueryGame
  | 1 -> QueryModemBufferSize
  | 2 -> QueryProtocolVersion
  | 3 -> QueryStonesOnBoard
  | 4 -> QueryBlackTime
  | 5 -> QueryWhiteTime
  | 6 -> QueryCharacterSet
  | 7 -> QueryRules
  | 8 -> QueryHandicap
  | 9 -> QueryBoardSize
  | 10 -> QueryTimeLimit
  | 11 -> QueryComputerPlayerColor
  | 12 -> QueryWhoAreYou
  | _ -> raise GmpParseFailed
;;

let color_of_move value =
  if (value land 0x200) = 0
  then `Black
  else `White

;;

let intersect_of_move value =
  value land 0b0111111111
;;

let extract_cmd msg = 
  let raw_cmd = extract_raw_cmd msg in
  let value = extract_raw_value msg in
  match raw_cmd with
    0 -> OkCmd (* fixme *)
  | 1 -> DenyCmd 
  | 2 -> NewGameCmd
  | 3 -> 
      let mask = (2 lsl 9) in
      let flag = (value land mask) in
      let value' = 
	value land (mask - 1) in
      if (flag = 1) then 
      (QueryExtendedCmd value')
      else (QueryCmd 
	      (extract_query value'))
  | 4 -> (AnswerCmd value)
  | 5 -> (MoveCmd
	    (color_of_move value,
	     intersect_of_move value))
  | 6 -> (TakebackMoveCmd value)
  | _ -> raise GmpParseFailed
;;
 
let unmarshall_message msg =
  {hsn = his_seqn_no msg;
   ysn = your_seqn_no msg;
   command = extract_cmd msg}
;;

(* End of parsing functions *)

(* Start of printing code *)

let gmp_query_to_string q =
  match q with
    QueryGame -> "game type"
  | QueryModemBufferSize -> "modem buffer size"
  |  QueryProtocolVersion -> "protocol version"
  |  QueryStonesOnBoard -> "number of stones on board"
  |  QueryBlackTime -> "black player time used"
  |  QueryWhiteTime -> "white player time used"
  |  QueryCharacterSet -> "character set"
  |  QueryRules -> "rule set"
  |  QueryHandicap -> "handicap" 
  |  QueryBoardSize -> "board size"
  |  QueryTimeLimit -> "time limit"
  |  QueryComputerPlayerColor -> "computer player color"
  |  QueryWhoAreYou -> "who are you"
;;

let command_to_string cmd =
  match cmd with
    OkCmd -> "Ok"
  | DenyCmd -> "Deny"
  | NewGameCmd -> "New game"
  | QueryExtendedCmd _ -> "Query for extended command support"
  | QueryCmd q -> "Query for " ^ (gmp_query_to_string q)
  | AnswerCmd i -> "Answer of " ^ (string_of_int i)
  | MoveCmd (`Black,i) -> "Black move at " ^ (string_of_int i)
  | MoveCmd (`White, i) -> "White move at " ^ (string_of_int i)
  | TakebackMoveCmd n -> "Take back " ^ (string_of_int n) ^ " moves"
;;

let sent_message_to_string msg = 
    "Gmp: SENT [lsn=" 
  ^ (string_of_int msg.ysn) 
  ^ ",rsn="
  ^ (string_of_int msg.hsn) 
  ^ "] "
  ^ (command_to_string msg.command)
;;

let recv_message_to_string msg = 
    "Gmp: RECV [lsn=" 
  ^ (string_of_int msg.hsn) 
  ^ ",rsn="
  ^ (string_of_int msg.ysn) 
  ^ "] "
  ^ (command_to_string msg.command)
;;


(* End of printing code *)

type raw_gmp_connection =
    { gmp_in : in_channel;
      gmp_out : out_channel }
;;

let init_connection (proc_in, proc_out) =
 {gmp_in = proc_in;
  gmp_out = proc_out }
;;

let add_to_log s =
  output_string log (s ^ "\n");
  flush log
;;

let handle_talk_text buffer n =
  let text = String.sub buffer
      4 (n-4) in
  if String.length text > 0 then begin
    Printf.printf "GMP says: '%s'\n" text;
    flush stdout
  end
;;

let read_message connection =
  let buffer = 
    String.create buffer_size in
  let n = ref 0 in
  while !n < 4 do 
    n := !n + input connection.gmp_in 
	buffer !n buffer_size;
  done;
  handle_talk_text buffer !n;
  let msg = 
    unmarshall_message buffer in 
  add_to_log 
    (recv_message_to_string msg);
  msg
;;

let encode_seqn msg =
  2 * msg.hsn
    + msg.ysn
;;

let encode cmd value = 
  let cmd' = cmd land 0b0111 in
  let top_value = value lsr 7 in
  let bottom_value = 
    value land 0b01111111 in
  let b1 = 0b10000000 lor
    (cmd' lsl 4) lor top_value in
  let b2 = 0b10000000 lor
    bottom_value in
  (char_of_int b1, char_of_int b2)
;;

let encode_query q = 
  match q with 
    QueryGame -> 0
  | QueryModemBufferSize -> 1
  | QueryProtocolVersion -> 2
  | QueryStonesOnBoard -> 3
  | QueryBlackTime -> 4
  | QueryWhiteTime -> 5
  | QueryCharacterSet -> 6
  | QueryRules -> 7
  | QueryHandicap -> 8
  | QueryBoardSize -> 9
  | QueryTimeLimit -> 10
  | QueryComputerPlayerColor -> 11
  | QueryWhoAreYou -> 12
;;

let sum = List.fold_left (+) 0;;

let checksum buffer =
  let indices = [0; 2; 3] in
  let values = List.map 
      (fun i -> 0x7f land (int_of_char
	     (String.get buffer i))) 
      indices in
  let total = sum values in
  0x80 lor (0x7f land total)
;;

let marshall_message msg =
  let buffer = String.create 4 in
  String.set buffer 0 
    (char_of_int (encode_seqn msg));
  let (b2,b3) = 
    match msg.command with
      OkCmd -> encode 0 0b1111111111
    | DenyCmd -> encode 1 0
    | NewGameCmd -> encode 2 0
    | (QueryExtendedCmd n) ->
	encode 3 (0x200 + n)
    | (QueryCmd q) -> 
	encode 3 (encode_query q)
    | (AnswerCmd n) -> encode 4 n
    | (MoveCmd (col,n)) ->
	let col_bit = match col with
	  `White -> 0x200
	| `Black -> 0
	in encode 5 (col_bit + n)
    | (TakebackMoveCmd n) ->
	encode 6 n
  in String.set buffer 2 b2;
  String.set buffer 3 b3;
  String.set buffer 1 
    (char_of_int (checksum buffer));
  buffer
;;


let send_message connection msg =
  let s = marshall_message msg in
  output connection.gmp_out s 0 4;
  flush connection.gmp_out;
  add_to_log 
    (sent_message_to_string msg)
;;

