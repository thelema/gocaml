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

type 'a board = 'a array array
type board_pos = (int * int)

let board_size b = Array.length b
let make_board s v = Array.make_matrix s s v
let make_board_pos s f =   
  Array.init s (fun i -> Array.init s (fun j -> f (i,j)))

let clone_board b = 
  let s = board_size b in
  Array.init s (fun i -> Array.copy b.(i))

let index arr (x,y) = arr.(x).(y)
let matrix_set arr (x,y) v = arr.(x).(y) <- v

let in_size s (x,y) = 
  (0 <= x) && (x < s) &&
  (0 <= y) && (y < s)

let next s (i,j) = if i+1 < s then (i+1,j) else (0,j+1) 
let dist s (x,y) (i,j) = i-x + s*(j-y)

let enum b = 
  let rec make start xs =
    let s = board_size xs in(*Inside the loop, as [make] may later be called with another array*)
    Enum.make
      ~next:(fun () ->
	       if in_size s !start then
		 let ret = !start, index xs !start in
		 start := next s !start;
		 ret
	       else
		 raise Enum.No_more_elements)
      ~count:(fun () ->	dist s (s,s) (!start))
      ~clone:(fun () -> make (ref !start) xs)
  in
  make (ref (0,0)) b 

let unsafe_pair_of_pos (x,y) = (x,y)

let pos_of_pair s (x,y) = 
  if in_size s (x,y)
  then (x,y)
  else failwith "off board"

let int_of_pos s (x,y) = s*x + y

exception Done

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
      
