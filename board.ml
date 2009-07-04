(* Board contents is represented as a map from x,y intersect to color. *)
open Common
   
type game_board = game_val board

let to_char gv own = 
  match (gv, own) with
  | `Empty, `Black -> "+"
  | `Empty, `White -> "-"
  | _, `Empty -> "."
  | `Black, `White -> "b"
  | `Black, `Black -> "B"
  | `White, `Black -> "w"
  | `White, `White -> "W"

module Group = struct
(** 
 * a group is a set of positions containing stones of the same color.
 * groups have liberties = adjacent empty positions.
 *)
  type t = {
      colr : game_val;
      mutable ownr : game_val;
      mutable frce : bool;
      mems : PosS.t;
      brdr : PosS.t;
    }
  let compare = Pervasives.compare

  let size g = PosS.cardinal g.mems
      
  let color g = g.colr
  let members g = g.mems
  let border g = g.brdr
  let owner g = g.ownr
  let is_dead g = g.ownr != g.colr

  let set_owner g o = g.ownr <- o

  let make_empty g = {g with colr = `Empty; ownr = `Empty}

  let is_forced g = g.frce

  let toggle_dead force g =
    if g.frce && (not force) 
    then print_string "can't toggle_dead forced group\n"
    else begin
      if g.colr = `Empty then () (* empty groups can't be dead *)
      else match g.ownr with
      | `Empty -> assert false (*only empty groups can be without owner*)
      | `Black -> g.frce <- force; g.ownr <- `White
      | `White -> g.frce <- force; g.ownr <- `Black
    end

  let kill force g = 
    if g.frce && (not force) && g.ownr = g.colr
    then print_string "can't kill forced alive group\n"
    else match g.colr with
      `Empty -> ()
    | #color as c ->
	g.frce <- force; 
	g.ownr <- ((opposite_color c) :> game_val)
	    
  let crop_brdr g = {g with brdr = PosS.diff g.brdr g.mems}
      
  let union g1 g2 = 
    assert (g1.ownr = g2.ownr && g1.colr = g2.colr);
    { g2 with
      mems = PosS.union g1.mems g2.mems;
      brdr = PosS.union g1.brdr g2.brdr;
    }

  let unit s c p = {
    colr = (c:>game_val);
    ownr = (c:>game_val);
    frce = false;
    mems = PosS.singleton p;
    brdr = nbrs s p |> poss_of_posl;
  }

  let eunit s p = {
    colr = `Empty;
    ownr = `Empty;
    frce = false;
    mems = PosS.singleton p;
    brdr = nbrs s p |> poss_of_posl;
  }

  let rec expand s gv_ g =
(*  print_string "expand: "; Group.print g; *)
    let nbrset p = nbrs s p |> poss_of_posl in
    let add_nbrs_of p a = PosS.union (nbrset p) a in
    let new_mems, brdr = PosS.partition (fun p -> gv_ p = g.colr) g.brdr in
    if PosS.is_empty new_mems then g
    else 
      let mems' = PosS.union g.mems new_mems in
      let nbrs = PosS.fold add_nbrs_of new_mems PosS.empty in
      let nbrs = PosS.diff nbrs mems' in
      let brdr' = PosS.union brdr nbrs in
      expand s gv_ {g with mems = mems'; brdr = brdr'}

  let liberties pos_is_lib g = PosS.filter pos_is_lib g.brdr

  let print end_char ?libs g = 
    print_string (to_char g.colr g.ownr);
    print_string " S:"; print_int (size g);
    print_string "B:"; print_int (PosS.cardinal g.brdr);
    match libs with
      None -> ()
    | Some pil -> 
	print_string "L:"; print_int (PosS.cardinal (liberties pil g));
    print_string end_char

  let score g (w,b) = 
    let pts = size g in
(*   Printf.printf "size: %d colr: %s ownr: %s (w%d,b%d)\n" 
      pts (gv_to_string g.colr) (gv_to_string g.ownr) w b; *)
    match color g, owner g with
    |      _, `Empty -> (w, b)
    | `Empty, `Black -> (w, b+pts)
    | `Empty, `White -> (w+pts, b)
    | `White, `Black -> (w, b+(2*pts))
    | `Black, `White -> (w+(2*pts), b)
    | `White, `White
    | `Black, `Black -> (w, b)

end

module GrpS : Set.S with type elt = Group.t = Set.Make (Group)

module Dragon = 
  struct
    type t = GrpS.t

    let color d = Group.color (GrpS.choose d)
	
    let toggle_dead force d = GrpS.iter (Group.toggle_dead force) d

    let kill force d = GrpS.iter (Group.kill force) d

    let union = GrpS.union

    let of_group = GrpS.singleton

    let to_group_list = GrpS.elements
	
    let list_to_group_list = List.fold_left (fun a d -> List.rev_append (to_group_list d) a) []

    let liberties is_lib d = GrpS.fold (fun g a -> PosS.union (Group.liberties is_lib g) a) d PosS.empty

    let score (w,b) d = GrpS.fold Group.score d (w,b)

    let size d = GrpS.fold (fun g a -> Group.size g + a) d 0

    let print ?libs post_string d = 
      print_string (if color d = `Black then "b::" else "w::");
      GrpS.iter (Group.print " " ?libs) d;
      print_string post_string

  end

type group_board = Group.t board

(* Fully annotated board type *)
type t = {
    game_board : game_board;
    group_board : group_board; 
    (* list of all groups *)
    mutable groups : Group.t list option;
  }

let is_color t c0 p = index t.game_board p = (c0 :> game_val)

let gv_at t p = index t.game_board p
let grp_at t p = index t.group_board p

let size t = board_size t.game_board

let _write_one t gv g p = 
  matrix_set t.game_board p gv;
  matrix_set t.group_board p g

let write_stone t c p = _write_one t (c:>game_val) (Group.unit (size t) c p) p

let write_empty t g = 
  PosS.iter (_write_one t `Empty (Group.make_empty g)) (Group.members g)

let write_group t g = 
  Group.members g 
    |> PosS.iter (fun p -> matrix_set t.group_board p g) 

let is_empty_or_dead t p = 
  match gv_at t p with
    `Empty -> true
  | #color -> Group.is_dead (grp_at t p)

let liberties t g = Group.liberties (is_empty_or_dead t) g
let liberties_d t d = Dragon.liberties (is_empty_or_dead t) d

let nbr_pos_of_group t g =
  let c = Group.color g in
  let ns = 
    (Group.border g) 
      |> PosS.filter (fun p -> index t.game_board p != c) in
  PosS.fold (fun p gs -> GrpS.add (grp_at t p) gs) ns GrpS.empty

type hasht = int

exception Illegal_move of string * board_pos

let enum t = enum t.game_board

let enum_stones t = 
  let f = function (_,`Empty) -> None | p,(#color as c) -> Some (p,c) in
  enum t |> Enum.filter_map f

type annot = { dead : bool; forced : bool; owner : game_val }

let get_annot t p = 
  let g = grp_at t p in
  {dead = Group.is_dead g; owner = Group.owner g; forced = Group.is_forced g}


let map trans arr =
  let s = Array.length arr in
  Array.init s (fun i -> Array.init s (fun j -> trans arr.(i).(j)))

(*
let map_mod trans arr =
  iterij (fun i j e -> arr.(i).(j) <- trans e) arr
*)

let on_board t p = in_size (size t) p

let all_pos b = Enum.fold (fun acc (p,_) -> PosS.add p acc) PosS.empty (enum b)
  
let handicap_stones s = 
  let (high,mid,low) = 
    match s with 
      B9  -> (6, 4, 2)
    | B11 -> (8, 5, 2)
    | B13 -> (9, 6, 3)
    | B19 -> (15, 9, 3)
  in 
  List.map (fun p -> pos_of_pair (int_of_boardsize s) p)
    [(high,high); (low,low); (high,low); (low,high);
     (mid,low); (mid,high); (low,mid); (high,mid)]
    
let center_stone n = ((n-1)/2, (n-1)/2)

let create bs ~handi:h = 
  let s = int_of_boardsize bs in 
  let t = {
   game_board = make_board s `Empty;
   group_board = make_board_pos s (Group.eunit s);
   groups = None;
  } in
  let do_moves = List.iter (write_stone t `Black) in
  let size = (int_of_boardsize bs) in
  let add_handicap_stones n = 
    let stones = List.take n (handicap_stones bs) in
    do_moves stones
  and add_center_stone () =
    do_moves [pos_of_pair size (center_stone size)]
  in
  if h mod 2 = 1 && h > 4 then begin
    add_center_stone ();
    add_handicap_stones (h-1);
  end else 
    add_handicap_stones h;
  t

let duplicate t = {t with 
		     game_board=clone_board t.game_board;
		     group_board=clone_board t.group_board}

(* Board printing functions *)
let square_to_string s pos_to_string = 
  let buf = Buffer.create 150 in
  let append s = Buffer.add_string buf s in
  append "  0 1 2 3 4 5 6 7 8\n";
  for i = 0 to s-1 do
    append (string_of_int i);
    append " ";
    for j = 0 to s-1 do
      append (pos_to_string i j);
      append " ";
    done;
    append "\n";
  done;
  Buffer.contents buf

let board_to_string t = 
  let s = size t in
  let pos_to_string i j = 
    let p = pos_of_pair s (i,j) in
    to_char (gv_at t p) (Group.owner (grp_at t p))
  in
  square_to_string s pos_to_string


(* End of board printing functions *)

let split3 c1 t = 
  let loop (l, f, e) p =
    match gv_at t p with
    | `Empty -> (PosS.add p l, f, e)
    | #color as c when c = c1 -> (l, GrpS.add (grp_at t p) f, e)
    | #color (* when c != c1*) -> (l, f, PosS.add p e)
  in
  List.fold_left loop (PosS.empty, GrpS.empty, PosS.empty)

(** finds groups neighboring point p with color c *)
let nbr_groups t p c = 
  nbrs (size t) p 
  |> List.filter (is_color t c)
  |> List.fold_left (fun gs a -> GrpS.add (grp_at t a) gs) GrpS.empty

(* Adds stone to board non-destructive *)
let add_stone t p c =
  let gv = try gv_at t p with 
    Invalid_argument _ -> (* thrown by array access out of range *)
      raise (Illegal_move ("Off the board", p))
  in
  match gv with
  | #color ->
      raise (Illegal_move ("Stone already there", p))
  | `Empty -> 
      let friends = nbr_groups t p c in
      let group = 
	Group.unit (size t) c p 
          |> GrpS.fold Group.union friends 
	  |> Group.crop_brdr (* remove any internal points 
				 (especially p) from the border *)
      in
      let t = duplicate t in
      write_stone t c p;
      write_group t group;
      t

(** remove group g from board t, returning modified board, sanity checking *)
let remove_group g t =
  assert (PosS.is_empty (liberties t g));
  Printf.printf "Killing %d stones: " (Group.size g);
  Group.print "\n" g;
  let t = duplicate t in
  write_empty t g;
  t

let to_kill t g = PosS.is_empty (liberties t g)

let killed_by t p c = 
  assert (is_color t c p);
  let opp_c = opposite_color c in
  nbr_groups t p opp_c
  |> GrpS.filter (to_kill t)

let make_move t p c = 
(*  print_string (board_to_string board); *)
  let t = add_stone t p c in
  let g = grp_at t p 
  and killed = killed_by t p c in
  if to_kill t g && GrpS.is_empty killed then
    raise (Illegal_move ("Suicide Move", p));
  let t = GrpS.fold remove_group killed t in
  print_string (board_to_string t); flush_all ();
  let k_count = 
    GrpS.fold (fun g acc -> acc + Group.size g) killed 0
  in
  (t, k_count)

let stones t = enum_stones t |> Enum.count

(* p,color list *)
let to_list t = enum_stones t |> List.of_enum

let hash t =
  let size = size t in
  let hashwalk acc (p,_) = (int_of_pos size p) + acc in
  enum_stones t |> Enum.fold hashwalk 0

let equal t1 t2 = t1.game_board = t2.game_board
    
(*
 *
   functions for taking a board and agreggating the stones into dragons
 *
 *)

(**
 * A dragon is a set of groups with the same color.  Two groups
 * that share a liberty are in the same dragon.
 * Eyes in a dragon must be owned by the dragon's color, and will 
 * have only the dragon's stones in their boundaries.
 *)
      
let rec make_groups t acc ps = 
  if PosS.is_empty ps then acc
  else 
    let p = PosS.choose ps in
    let seed = Group.unit (size t) (gv_at t p) p in
    let g = Group.expand (size t) (gv_at t) seed in
    write_group t g;
    make_groups t (g :: acc) (PosS.diff ps (Group.members g))
      
let has_common_liberties t g d = 
  let l1 = liberties t g
  and l2 = liberties_d t d in
  not (PosS.is_empty (PosS.inter l1 l2))

let cluster near union unit groups =
  let insert dragons item =
    let nears, fars = List.partition (near item) dragons in
    let new_grp = List.fold_left union (unit item) nears in
    new_grp :: fars
  in
  List.fold_left insert [] groups

let own_empty t d = 
  let g = GrpS.choose d in 
  if Group.color g = `Empty then 
(* empty dragons have only one group, so we're working on the whole dragon *)
    let ownr_of_pos p = Group.owner (grp_at t p) in
    let test_ownr = ownr_of_pos (PosS.choose (Group.border g)) in
    let owner = 
      if PosS.for_all (fun p -> ownr_of_pos p = test_ownr) (Group.border g)
      then test_ownr
      else `Empty
    in
    Group.set_owner g owner

(* modifies the board, making groups of empty regions *)
let make_dragons t gl = 
  let near g d = 
    Dragon.color d = Group.color g 
      && has_common_liberties t g d
  in
  let dl = cluster near Dragon.union Dragon.of_group gl in
  List.iter (own_empty t) dl;
  dl

let finalize t =
  match t.groups with
    None -> 
      let gl = make_groups t [] (all_pos t) in
      t.groups <- Some gl;
      gl
  | Some gl -> 
      gl
	
let annotate t =
(* a dragon is dead if it has one liberty *)
  let ataridead d = PosS.cardinal (liberties_d t d) < 2
(* a dragon can't be alive if it's got < 6 stones *)
  and _smalldragdead d = Dragon.size d < 6
(* a dragon is probably dead if it doesn't have internal liberties *)
  and internaldead d = 
    PosS.exists (fun p -> Group.color (grp_at t p) = Dragon.color d) (liberties_d t d)
(* add more tests for being dead here *)

  and kill f dl = 
    let dead,_ = List.partition f dl in
    List.iter (Dragon.kill false) dead;
    let gl = Dragon.list_to_group_list dl in
    make_dragons t gl
  in
  finalize t
  |> make_dragons t
  |> kill ataridead
(*  |> kill smalldragdead*)
  |> kill internaldead
    
let score t (wc, bc) = (* white captures, black captures *)
  let (wp,bp) = 
    annotate t |> List.fold_left Dragon.score (wc, bc) 
  in
  print_string (board_to_string t);
  Printf.printf "W:%d.5 B:%d\n" wp bp;
  (wp, bp)

let forcetoggle t p = 
  print_string "Killing at "; print_pos p; print_endline "";
  Group.toggle_dead true (grp_at t p);
  print_string (board_to_string t);
  annotate t |> List.iter (Dragon.print ~libs:(is_empty_or_dead t) "\n")


