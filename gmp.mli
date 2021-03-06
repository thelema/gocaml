open Common
(* High level GMP interface, using Rawgmp to do the parsing stuff but
   additionally hiding all the sequence number stuff *)

exception UnexpectedCommand of Rawgmp.gmp_command

type gmp_connection

val init_connection : (IO.input * unit IO.output) -> gmp_connection

val read_command : gmp_connection -> Rawgmp.gmp_command
val receive_move : gmp_connection -> move

val send_ok : gmp_connection -> unit
val wait_for_ok : gmp_connection -> bool -> unit

val send_deny : gmp_connection -> unit
val send_answer : gmp_connection -> int -> unit

val send_newgame : gmp_connection -> int option -> int -> color -> unit

val send_move : gmp_connection -> move -> unit

val perform_query : gmp_connection -> Rawgmp.gmp_query -> int
val respond_to_queries : gmp_connection -> unit
