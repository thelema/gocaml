let handicap = ref None;;
let color = ref Board.Black;;
let play_local = ref false;;
let board_size = ref 9;;
let winsize = ref 200;;
let gmp_prog = ref "gnugo --mode=gmp --quiet -o log-sgf.txt --score all 2>log-gnugo.txt";;

let options = 
  [("-handicap", 
    Arg.Int (fun n -> 
      if n < 2 then raise (Arg.Bad "Handicap must be 2 or greater")
      else handicap := (Some n)),
    "Sets blacks advantage");

   ("-playlocal",
    Arg.Set play_local,
    "Play locally (human vs human)");

   ("-color",
    Arg.String (fun s -> match s with
      "white" -> color := Board.White
    | "black" -> color := Board.Black
    | _ -> raise (Arg.Bad "Color must be 'black' or 'white'")), 
    "Chooses local color");

   ("-boardsize",
    Arg.Int (fun n -> match n with
      9 | 11 | 13 | 19 -> board_size := n
    | _ -> raise (Arg.Bad "Board size must be 9, 11, 13 or 19")),
    "Sets the board size");

   ("-winsize", Arg.Int (fun n -> winsize := n), 
    "Sets window dimensions");

   ("-gmpprog", Arg.String (fun s -> gmp_prog := s), 
    ("Sets GMP-compatible program to use (default is " ^ !gmp_prog));
];;

let errmsg = "Gocaml v1.0.\n\nCommand line options are:";;

let parse_command_line () =
  Arg.parse options ignore errmsg;
;;

