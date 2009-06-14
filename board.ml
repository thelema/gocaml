(* Board contents is represented as a map from x,y intersect to color. *)
open Global
   
type board = game_val array array

let bv_ b p = index b p

let is_color b c0 p = bv_ b p = (c0 :> game_val)

let isnot_gv b gv p = bv_ b p != gv

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
    brdr = nbrs s p ==> poss_of_posl;
  }

  let eunit s p = {
    colr = `Empty;
    ownr = `Empty;
    frce = false;
    mems = PosS.singleton p;
    brdr = nbrs s p ==> poss_of_posl;
  }

  let rec expand s gv_ g =
(*  print_string "expand: "; Group.print g; *)
    let nbrset p = nbrs s p ==> poss_of_posl in
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

type t = {
    b : board;
    ga : Group.t array array;
    mutable gl : Group.t list option;
  }

let size t = Array.length t.b

let set_grp1 t g p = matrix_set t.ga p g

let write2 t gv g p = 
  let write1 t gv p = matrix_set t.b p gv in
  write1 t gv p; 
  set_grp1 t g p

let gv_ t p = index t.b p
let grp_ t p = index t.ga p

let write t gv g ps = PosS.iter (write2 t gv g) ps

let write_empty t g = write t `Empty (Group.make_empty g) (Group.members g)
let write_stone t c p = write2 t (c:>game_val) (Group.unit (size t) c p) p

let set_group t g = PosS.iter (set_grp1 t g) (Group.members g)

let is_empty_or_dead t p = 
  match gv_ t p with
    `Empty -> true
  | #color -> Group.is_dead (grp_ t p)

let liberties t g = Group.liberties (is_empty_or_dead t) g

let liberties_d t d = Dragon.liberties (is_empty_or_dead t) d

let nbr_stones t g = 
  PosS.filter (isnot_gv t.b (Group.color g)) (Group.border g)

let nbr_groups t g =
  let ns = nbr_stones t g in
  PosS.fold (fun p gs -> GrpS.add (grp_ t p) gs) ns GrpS.empty

type hasht = int

exception Illegal_move of string * board_pos

let fold f t i = walk (fun p acc -> f acc (gv_ t p, grp_ t p)) (size t) i
let fold_gv f t i = walk (fun p acc -> f acc (gv_ t p)) (size t) i

let walkstones t funacc init =
  let funint p acc =
    match gv_ t p with
    | `Empty -> acc
    | #color as c -> funacc p c acc
  in
  walk funint (size t) init
    
let itersquare f t = walk (fun p _ -> f p) (size t) ()

let iterboard funstone funempty t =
  let f p = 
    match gv_ t p with
    | `Empty -> funempty p
    | #color as c -> funstone p c
  in
  itersquare f t

let iterstones f t = iterboard f (fun _ -> ()) t

let b_iterij f t = itersquare (fun p -> f p (gv_ t p)) t
let ga_iterij f t = itersquare (fun p -> f p (grp_ t p)) t
let iterij2 f2 t =
  let f p = f2 p (gv_ t p) (grp_ t p) in
  itersquare f t

let iterij f = 
  Array.iteri (fun i row -> Array.iteri (fun j -> f i j) row)

let map trans arr =
  let s = Array.length arr in
  Array.init s (fun i -> Array.init s (fun j -> trans arr.(i).(j)))

let map_mod trans arr =
  iterij (fun i j e -> arr.(i).(j) <- trans e) arr

let on_board t p = in_size (size t) p

let all_pos s = walk (fun p acc -> PosS.add p acc) s PosS.empty
  
let init_matrix_pos s f = 
  Array.init s (fun i -> Array.init s (fun j -> f (pos_of_pair s (i,j))))

let empty bs = 
  let s = int_of_boardsize bs in 
  {
   b = Array.make_matrix s s `Empty;
   ga = init_matrix_pos s (Group.eunit s);
   gl = None;
 }

let duparr arr = 
  let s = Array.length arr in
  Array.init s (fun i -> Array.copy arr.(i))

let duplicate t = {t with b=duparr t.b;ga=duparr t.ga}

(* Board printing functions *)
let square_to_string s pos_to_string = 
  let buf = Buffer.create 150 in
  let append s = Buffer.add_string buf s in
  append "  0 1 2 3 4 5 6 7 8\n";
  for i = 0 to s-1 do
    append (string_of_int i);
    append " ";
    for j = 0 to s-1 do
      let e = pos_to_string i j in
      append e;
      append " ";
    done;
    append "\n";
  done;
  Buffer.contents buf

(*let board_to_string (b,_) = square_to_string (Array.length b) 
    (fun i j -> gv_to_string b.(i).(j))
*)
let board_to_string t = 
  let s = size t in
  let pos_to_string i j = 
    let p = pos_of_pair s (i,j) in
    to_char (gv_ t p) (Group.owner (grp_ t p))
  in
  square_to_string s pos_to_string


(* End of board printing functions *)

(* adds stone to board modifying board (no checks) *)
let add_stone_mod t p c = write_stone t c p

let split3 c1 t = 
  let loop (l, f, e) p =
    match gv_ t p with
    | `Empty -> (PosS.add p l, f, e)
    | #color as c when c = c1 -> (l, GrpS.add (grp_ t p) f, e)
    | #color as c (* when c != c1*) -> (l, f, PosS.add p e)
  in
  List.fold_left loop (PosS.empty, GrpS.empty, PosS.empty)

(* Adds stone to board non-destructive *)
let add_stone t p c =
  let gv = try gv_ t p with 
    Invalid_argument _ -> (* thrown by array access out of range *)
      raise (Illegal_move ("Off the board", p))
  in
  match gv with
  | #color ->
      raise (Illegal_move ("Stone already there", p))
  | `Empty -> 
      let friends = 
	nbrs (size t) p 
	  ==> List.filter (fun p -> gv_ t p = (c:>game_val))
	  ==> List.fold_left (fun a p -> GrpS.add (grp_ t p) a) GrpS.empty
      in
      let group = 
	Group.unit (size t) c p 
	  ==> GrpS.fold Group.union friends 
	  ==> Group.crop_brdr (* remove any internal points 
				 (especially p) from the border *)
      in
      let t = duplicate t in
      add_stone_mod t p c;
      set_group t group;
      t

(** finds groups neighboring point p with color c *)
let nbr_grps t p c = 
  nbrs (size t) p 
    ==> List.filter (is_color t.b c)
    ==> List.fold_left (fun gs a -> GrpS.add (grp_ t a) gs) GrpS.empty

(** remove group g from board t, modifying t, no checks *)
let remove_group_mod t g = write_empty t g
    
(** remove group g from board t, returning modified board, sanity checking *)
let remove_group g t =
  assert (PosS.is_empty (liberties t g));
  Printf.printf "Killing %d stones: " (Group.size g);
  Group.print "\n" g;
  let t = duplicate t in
  remove_group_mod t g;
  t

let to_kill t g = PosS.is_empty (liberties t g)

let killed_by t p c = 
  assert (is_color t.b c p);
  let opp_c = opposite_color c in
  nbr_grps t p opp_c
    ==> GrpS.filter (to_kill t)

let make_move t p c = 
(*  print_string (board_to_string board); *)
  let t = add_stone t p c in
  let g = grp_ t p 
  and killed = killed_by t p c in
  if to_kill t g && GrpS.is_empty killed then
    raise (Illegal_move ("Suicide Move", p));
  let t = GrpS.fold remove_group killed t in
  print_string (board_to_string t); flush_all ();
  let k_count = 
    GrpS.fold (fun g acc -> acc + Group.size g) killed 0
  in
  (t, k_count)

let stones board =
  let countfun _ _ acc = acc + 1 in
  walkstones board countfun 0

(* p,color list *)
let to_list t = 
  let collect p c acc = (p, c) :: acc in
  walkstones t collect []

let hash t =
  let size = size t in
  let hashwalk p _ acc = (int_of_pos size p) + acc in
  walkstones t hashwalk 0

let equal t1 t2 = t1.b = t2.b
    
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
    let seed = Group.unit (size t) (gv_ t p) p in
    let g = Group.expand (size t) (gv_ t) seed in
    set_group t g;
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
    let ownr_of_pos p = Group.owner (grp_ t p) in
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
  match t.gl with
    None -> 
      let gl = make_groups t [] (all_pos (size t)) in
      t.gl <- Some gl;
      gl
  | Some gl -> 
      gl
	
let annotate t =
(* a dragon is dead if it has one liberty *)
  let ataridead d = PosS.cardinal (liberties_d t d) < 2
(* a dragon can't be alive if it's got < 6 stones *)
  and smalldragdead d = Dragon.size d < 6
(* a dragon is probably dead if it doesn't have internal liberties *)
  and internaldead d = 
    PosS.exists (fun p -> Group.color (grp_ t p) = Dragon.color d) (liberties_d t d)
(* add more tests for being dead here *)

  and kill f dl = 
    let dead,alive = List.partition f dl in
    List.iter (Dragon.kill false) dead;
    let gl = Dragon.list_to_group_list dl in
    make_dragons t gl
  in
  finalize t
    ==> make_dragons t
    ==> kill ataridead
(*  ==> kill smalldragdead*)
    ==> kill internaldead
    
let score t (wc, bc) = (* white captures, black captures *)
  let (wp,bp) = 
    annotate t ==> List.fold_left Dragon.score (wc, bc) 
  in
  print_string (board_to_string t);
  Printf.printf "W:%d.5 B:%d\n" wp bp;
  (wp, bp)

let forcetoggle t p = 
  print_string "Killing at "; print_pos p; print_endline "";
  Group.toggle_dead true (grp_ t p);
  print_string (board_to_string t);
  annotate t ==> List.iter (Dragon.print ~libs:(is_empty_or_dead t) "\n")


