(* Board contents is represented as a map from x,y intersect to color. *)
   
module Intersect = 
  struct
    type t = {x: int; y: int}
    let compare = Pervasives.compare
  end
;;

open Intersect;;

module IntersectMap = 
  Map.Make(Intersect);;

type color = Black | White;;

type move = 
    Move of int * int * color
  | Pass of color;;

type t = color IntersectMap.t;;

let empty = IntersectMap.empty;;

(* Board printing functions *)
let intersect_to_string board i = 
  try
    match IntersectMap.find i board with
      White -> "W"
    | Black -> "B"
  with
    Not_found -> "+"
;; 

let board_to_string board = 
  let s = ref "" in
  s := !s ^ "  0 1 2 3 4 5 6 7 8\n";
  for i = 0 to !Options.board_size - 1 do
    s := !s ^ (string_of_int i) ^ " ";
    for j = 0 to !Options.board_size - 1 do
      let e = intersect_to_string 
	  board {x=i; y=j} in
      s := !s ^ e ^ " "
    done;
    s := !s ^ "\n";
  done;
  !s
;;

(* End of board printing functions *)

exception Illegal_move of string * move;;

(* Adds stone to board non-destructive *)
let add_stone board move =
   match move with 
     Pass _ -> board
   | Move (x,y,c) ->
       let i = {x=x;y=y} in
       if IntersectMap.mem i board
       then raise 
	   (Illegal_move 
	      ("Stone already there",
	       move))
       else IntersectMap.add i c board
;;

(* Extract the (x,y) of all the correctly colored stones *)
let make_stone_list board color =
  IntersectMap.fold
    (fun key data res ->
      if data = color then key :: res
      else res
    ) board []
;;

(* Clique finding functions *)

let on_board (x,y) =
  (x >= 0) && (x < !Options.board_size) &&
  (y >= 0) && (y < !Options.board_size);;

let next_to a b =
  let hdiff = abs (a.x - b.x) in
  let vdiff = abs (a.y - b.y) in
  (on_board (a.x,a.y)) && 
  (on_board (b.x,b.y)) &&
  (hdiff + vdiff) = 1
;;

let next_to_any stones e = 
  List.exists (next_to e) stones;;

let rec find_clique clique stones =
  let (neighbours,stones') = List.partition (next_to_any clique) stones in
  if List.length neighbours = 0 
  then (clique,stones)
  else find_clique (neighbours @ clique) stones'
;;

let rec split_into_cliques stones =
  match stones with 
    [] -> []
  | x::xs ->
      let (c,xs') = find_clique [x] xs
      in c :: (split_into_cliques xs')
;;

(* Liberty counting functions *)

let intersect_empty board (x,y) =
  not (IntersectMap.mem {x=x;y=y} board)
;;

let liberties_of_stone board i = 
  let intersect_is_liberty (x,y) = 
    if on_board (x,y) && 
      intersect_empty board (x,y)
    then 1 else 0 
  in
  intersect_is_liberty (i.x+1,i.y) +
  intersect_is_liberty (i.x-1,i.y) +
  intersect_is_liberty (i.x,  i.y+1) +
  intersect_is_liberty (i.x,  i.y-1)
;;

let sum = List.fold_left (+) 0;;

let liberties_of_clique board clique =
  sum (List.map (liberties_of_stone board) clique)
;;
(* End of liberty calculations *)

let opposite_color c = 
  match c with
    Black -> White
  | White -> Black
;;

(* Return all the cliques which are dead (no liberties) on the board
   of the given color *)

let dead_cliques board color =
  let all_stones = make_stone_list board color in
  let cliques = split_into_cliques all_stones in
  List.filter 
    (fun c -> (liberties_of_clique board c)=0)
    cliques
;;

let remove_stones board l =
  List.fold_left
    (fun board i -> IntersectMap.remove i board)
    board l
;;

(* Some helper functions to debug board stuff *)

let stones_on_board board =
  IntersectMap.fold
    (fun key data res -> res + 1)
    board 0;;

let make_move board move =
  match move with
    Pass _ -> board
  | Move (x,y,color) ->
      let opp_color = opposite_color color in
      let board' = add_stone board move in
      let killed_stones = dead_cliques board' opp_color in
      let board'' = remove_stones board' (List.flatten killed_stones) in
      let suicide_cliques = dead_cliques board'' color in

      if (List.length suicide_cliques) != 0
      then raise (Illegal_move ("Suicide move", move));
      board''
	;;

(* Ick, no Map.size in stdlib! *)
let stones board =
  IntersectMap.fold
    (fun k d v -> v + 1)
    board
    0
;;

(* x,y,color list *)    
let to_list board = 
  IntersectMap.fold
    (fun isect col results -> (isect.x,isect.y,col)::results)
    board []
;;

(* Still to do !!! *)
let score board =
  let white = 0 in
  let black = 0 in
  (white,black)
;;

let hash board =
  IntersectMap.fold
  (fun isect col results -> results + (!Options.board_size * isect.x) + isect.y)
  board 0
;;

let equal b1 b2 =
  if hash b1 != hash b2 
  then false
  else begin
    let l1 = List.sort compare (to_list b1) in
    let l2 = List.sort compare (to_list b2) in
    if List.length l1 != List.length l2
    then false 
    else (List.for_all2 (fun a b -> a = b) l1 l2)
  end
;;
