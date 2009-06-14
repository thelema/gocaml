(* Utilties *)
    
let is_odd n = (n mod 2) = 1;;

let rec take n l =
  if n = 0 then []
  else match l with
    x::xs -> x::(take (n-1) xs)
  | [] -> []
;;


let build_moves = List.map (fun (x,y) -> Board.Move (x,y,Board.Black));;
 
let premier_stones () =
  let (high,low) = 
    match !Options.board_size with
      9  -> (6,2)
    | 11 -> (8,2)
    | 13 -> (9,3)
    | 19 -> (15,3)
    | _ -> failwith "Bad board size" in
  build_moves [(high,high); (low,low); (high,low); (low,high)]
;;

let secondary_stones () = 
  let (high,mid,low) = 
    match !Options.board_size with 
      9  -> (6, 4, 2)
    | 11 -> (8, 5, 2)
    | 13 -> (9, 6, 3)
    | 19 -> (15, 9, 3)
    | _ -> failwith "Bad board size"
  in build_moves [(mid,low); (mid,high); (low,mid); (high,mid)]

;;

let center_stone () =
  let n = !Options.board_size - 1 in 
  Board.Move (n/2, n/2, Board.Black)
;;

let rec add_handicap history n =
  let do_moves = List.fold_left History.make_move in

  let add_premier_stones h n = 
    let stones = take n (premier_stones ()) in
    do_moves h stones in

  let add_center_stone h =
    History.make_move h (center_stone ()) in

  let add_secondary_stones h n =
    let stones = take n (secondary_stones ()) in 
    do_moves h stones in

  if n <= 4 then 
    add_premier_stones history n
  else if is_odd n then 
    let h' = add_center_stone history
    in add_handicap h' (n-1)
  else
    let h' = add_secondary_stones history (n-4) in
    add_handicap h' 4
;;

let setup n =
  add_handicap History.empty n;;

