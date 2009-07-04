type board_size = B9 | B11 | B13 | B19

let int_of_boardsize = function
    B9 -> 9
  | B11 -> 11
  | B13 -> 13
  | B19 -> 19

let boardsize_of_int = function
    9 -> B9
  | 11 -> B11
  | 13 -> B13
  | 19 -> B19
  | _ -> failwith "invalid boardsize in boardsize_of_int"

(*type 'a bs_pos = (int * int) constraint 'a = board_size *)

let in_size s (x,y) = 
  (0 <= x) && (x < s) &&
  (0 <= y) && (y < s)

type board_pos = (int * int)

let unsafe_pair_of_pos (x,y) = (x,y)

let pos_of_pair s (x,y) = 
  if in_size s (x,y)
  then (x,y)
  else failwith "off board"

let int_of_pos s (x,y) = s*x + y

exception Done

let walk funacc size init =
  let next (i,j) = 
    if i+1 < size then (i+1,j)
    else if j+1 < size then (0,j+1) 
    else raise Done in
  let rec loop p acc =
    try 
      let np = next p in (* fails at last position *)
      loop np (funacc p acc)
    with Done -> acc
  in
  loop (0,0) init

(* build neighbor lists for each position on the board *)
let (n9, n11, n13, n19) =  
  let build s = 
    let make_list (x, y) = 
      List.filter (in_size s) [(x+1,y); (x-1,y); (x,y+1); (x,y-1)] 
    in
    Array.init s (fun i -> Array.init s (fun j -> make_list (i,j)))
  in
  (build 9, build 11, build 13, build 19)

(* return the nbr map for a certain size board *)
let nbr = function
    9 -> n9 | 11 -> n11 | 13 -> n13 | 19 -> n19
  | _ -> failwith "Invalid board size in nbr"

(* return the list of neighbors on a certain size board at pos x,y *)
let nbrs s (x,y) = (nbr s).(x).(y)
  
module PosS = Set.Make(struct type t = board_pos let compare = compare end)
  (* pos set of pos list *)    
let poss_of_posl = List.fold_left (fun ls p -> PosS.add p ls) PosS.empty
  
let sprint_pos (x,y) = Printf.sprintf "(%d, %d)" x y
let print_pos p = print_string (sprint_pos p)
  
let index arr (x,y) = arr.(x).(y)
let matrix_set arr (x,y) v = arr.(x).(y) <- v
  
type color = [`Black | `White]
    
type game_val = [color | `Empty]
    
let opposite_color = function 
    `Black -> `White
  | `White -> `Black
      
type action = 
    Click of board_pos
  | PassMove
  | Quit
      
type move = 
    Move of board_pos * color
  | Pass of color
      
