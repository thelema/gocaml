exception GmpParseFailed

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

type gmp_command = 
  | OkCmd
  | DenyCmd 
  | NewGameCmd
  | QueryExtendedCmd of int
  | QueryCmd of gmp_query
  | AnswerCmd of int
  | MoveCmd of Common.color * int
  | TakebackMoveCmd of int

type raw_gmp_message = 
    { hsn : int;
      ysn : int;
      command : gmp_command;
    } 

val buffer_size : int

type raw_gmp_connection

val init_connection :
    (IO.input * unit IO.output) -> raw_gmp_connection

val read_message :
    raw_gmp_connection -> raw_gmp_message

val send_message :
    raw_gmp_connection -> raw_gmp_message -> unit
 
val command_to_string : gmp_command -> string

val add_to_log : string -> unit
