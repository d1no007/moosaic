open Core.Std

type order = Equal | Less | Greater
type point = float * float * float

(*for now, image is just a string*)
type image = string 

module type KDTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt 

  (* What this type actually looks like is left up to the
   * particular KDTREE implementation (i.e. the struct) *)
  type tree
  
  (* Returns an empty tree *)
  val empty : tree

  (* Insert elt into tree*)
  val insert : elt -> tree -> tree

  (* Balances a tree so that a nearest neighbor 
   * search ban be run on it *)
  val build_balanced: elt list -> int -> tree ->  tree 

  (* Search a KD tree for the given value and returns
  * the nearest neighbor *) 
  val nearest_neighbor : elt -> tree -> elt * float 

  (* Run invariant checks on the implementation of this KD tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end



module KDTree: KDTREE with type elt = point*image =
struct
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in
   * this is the only place you explicitly need to use C.t; you
   * should use elt everywhere else *)
  type elt = (float*float*float)*image

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt * tree

  (* Representation of the empty tree *)
  let empty = Leaf
  
  let compareR x y = 
    let ((r1,_,_),_),((r2,_,_),_) = x,y in 
      if r1 > r2 then Greater 
      else if r1 < r2 then Less
      else Equal 

  let compareG x y = 
    let ((_,g1,_),_),((_,g2,_),_) = x,y in 
      if g1 > g2 then Greater 
      else if g1 < g2 then Less
      else Equal 


  let compareB x y = 
    let ((_,_,b1),_),((_,_,b2),_) = x,y in 
      if b1 > b2 then Greater 
      else if b1 < b2 then Less
      else Equal 
   
  let compare i x y = 
    match (i mod 3) with 
    | 0 -> compareR x y 
    | 1 -> compareG x y 
    | 2 -> compareB x y 
    | _ -> failwith "shouldn't return any other value"

  (* Insert function for KD trees *)
  let insert (x : elt) (t : tree) : tree = 
    (* helper function with counter to determine 
     * what level we're on each time and what 
     * element we want to compare *)
    let rec helper (e: elt) (k: tree) (c: int) = 
      match k with
      | Leaf -> Branch (Leaf, e, Leaf) 
      | Branch (l, lst, r) ->
          match compare c e lst with   
          | Less -> Branch(helper e l (c+1), lst, r)
          | Greater -> Branch(l, lst, helper e r (c+1))
          | Equal -> Branch(l, lst, helper e r (c+1))        
  in helper x t 0 
  
  (* functions for determining  max of an int to help with
   * balanced function *)
  let max x y = if x > y then x else y 
  let min x y = if x < y then x else y 
  
  (* function to determine max depth of tree *)
  let max_depth (t: tree) : int =
    let rec max_dep (m: int) (lst: (tree*int) list) = 
    (* m is the max, and the int in the tree*int list
     * is the current depth. we compare the current 
     * max to the current depth and continue traversing
     * through adding 1 to the current depth of each 
     * tree *) 
      match lst with 
      | [] -> m 
      | (Leaf,c)::tl -> max_dep (max m c) tl 
      | (Branch(l,lst,r), c)::tl -> max_dep (max m c) ((l, c+1)::(r,c+1)::tl)
    in max_dep 0 [(t,0)]

  (*funtion to determine min depth of a tree*)
  let min_depth (t: tree) : int = 
    let rec min_dep (m: int) (lst: (tree*int) list) = 
    match lst with 
    | [] -> m 
    | (Leaf,c) :: tl -> if c < m then min_dep c tl else min_dep m tl 
    | (Branch(l,lst,r),c)::tl -> min_dep m ((l, c+1)::(r,c+1)::tl)
  in min_dep (max_depth t) [(t,0)]

  (* function to check if tree is balanced - checks to see if 
   * min_depth and max_depth are one apart or equal *)
  let is_balanced (t: tree) : bool = 
    if abs((max_depth t) - (min_depth t)) <= 1 then true
    else false 

  (* function that will help sort by R value *)
  let rsort (x: elt) (y: elt): int = 
    match compare 0 x y with 
    | Equal -> 0 
    | Greater -> 1 
    | Less -> -1 

  (* retrieves all the elements in a tree *)
  (*
  let rec retrieve_elements (t1: tree) : elt list = 
    match t1 with 
    | Leaf -> []
    | Branch(l,lst,r) -> lst @ (retrieve_elements l) @ (retrieve_elements r) 
   *) 
  (* sorts all the elements in the tree by R color component *)
  let sort_list (lst: elt list) : elt list = 
    List.sort rsort lst 
  
  (* finds the median of a sorted list *)
  let find_median (lst: elt list) : elt  = 
    let length = List.length lst in 
      if length % 2 = 0 then 
        let middle = length / 2 in 
          match List.nth lst middle with 
          | Some m -> m 
          | None -> failwith "not enough elements" 
      else let middle = (length -1) / 2 in 
          match List.nth lst middle with 
          | Some m -> m 
          | None -> failwith "not enough elements" 
   let sort (x: elt) (y: elt) (c: int): int = 
    match compare c x y with 
    | Equal -> 0 
    | Greater -> 1 
    | Less -> -1 

  (* sorts all the elements in the tree by R color component *)
  let sort_list (lst: elt list) (c: int) : elt list = 
    List.sort (fun x y -> sort x y c) lst 
  
  (*find what index the median of a sorted list is at*) 
  let index (lst: elt list) : int =
      let length = List.length lst in 
        if length % 2 = 0 then 
            length / 2
        else (length - 1) / 2 
    
  (* finds the median of a sorted list *)
  let find_median (lst: elt list) : elt  =  
      match List.nth lst (index lst) with 
      | Some m -> m 
      | None -> failwith "not enough elements" 
  
  let rec divide_lists lst left right middle c median = 
    match lst with 
    | [] -> (left, right) 
    | hd :: tl -> if c < median then 
                    divide_lists tl (hd :: left) right middle (c+1) median 
                  else if c > median then 
                    divide_lists tl left (hd :: right) middle (c+1) median
                  else 
                    divide_lists tl left right (hd :: middle) (c+1) median 
                    
  (* builds a balanced tree *)
  let rec build_balanced (lst: (elt list)) (c: int) (t: tree) : tree = 
    match lst with 
    | [] -> t
    | _ -> let sorted_lst = sort_list lst c in 
           let new_node = find_median sorted_lst in 
           let (left_list, right_list) = 
            divide_lists (sorted_lst) [] [] [] 0 (index lst) 
           in Branch((build_balanced left_list (c+1) empty),
            new_node, (build_balanced right_list (c+1) empty))
           
           
  (* balances a tree *)
  (*
  let balance (t: tree) : tree = 
    if is_balanced t then t 
    else 
      let lst = sort_list (retrieve_elements t) in 
      let root_node = find_median lst in 
      let new_tree = insert root_node empty in 
      let new_list = List.filter ~f:(fun x -> x <> root_node) lst 
      in List.fold_right ~f: insert ~init: new_tree lst 
    *) 
(*function for raising something to a power*)

(*calculates the distance between two points*)
let distance (e1: elt) (e2: elt) : float =
    let ((x,y,z),_),((x1,y1,z1),_) = e1,e2 in 
        ((x-.x1)**2.) +. ((y-.y1)**2.) +. ((z-.z1)**2.)

(*calculates the distance between a point and the 
 * hyperplane--used to see if other side of tree
 * needs to be traversed as well *)         
let distance_to_plane (c: int) (e1: elt) (e2: elt) = 
    match (c mod 3) with 
    | 0 -> let ((r,_,_),img) = e2 in distance e1 ((r,0.,0.),img) 
    | 1 -> let ((_,g,_),img) = e2 in distance e1 ((0.,g,0.),img) 
    | 2 -> let ((_,_,b),img) = e2 in distance e1 ((0.,0.,b),img) 
    | _ -> failwith "shouldn't be any other value!" 

(* Given a test point and a tree, finds the closest match to test point *)
  let rec nearest_neighbor (original : elt) (t : tree) : elt * float = 
   let rec traverse (o: elt) (current_best: elt) (k: tree) (d: float) (c: int): 
        elt * float =
        match k with 
        | Leaf -> (current_best, d)
        | Branch (l, hd, r) -> 
            let (current_best, d) = if distance hd o < d then 
                (hd, distance hd o) else (current_best, d) in
            match compare c o hd with
                | Less -> 
                    let (current_best, d) = traverse o current_best l d (c+1) in
                    if distance_to_plane c o hd <= d then
                    traverse o current_best r d (c+1) else (current_best, d)
                | Greater | Equal -> 
                    let (current_best, d) = traverse o current_best r d (c+1) in
                    if distance_to_plane c o hd <= d then 
                    traverse o current_best l d (c+1) else (current_best,d) in 
  match t with 
  | Leaf -> failwith "there are no nodes in this tree" 
  | Branch(_, hd, _) -> traverse original hd t (distance hd original) 0 
  
  
  (* string functions using for debugging *) 
  let string_of_ans (e: elt) (t: tree)= 
    match nearest_neighbor e t with 
    | (((r,g,b),img), f) -> Pervasives.string_of_float r ^ 
                                Pervasives.string_of_float g ^ 
                                Pervasives.string_of_float b ^ " " ^ img ^ " "^
                                Pervasives.string_of_float f  
            
  let test_build_balanced () = 
    let e1 = ((3.0,1.0,4.0),"no") in 
    let e2 = ((2.0,3.0,7.0),"no") in
    let e3 = ((4.0,3.0,4.0),"no") in
    let e4 = ((2.0,4.0,5.0),"no") in
    let e5 = ((6.0,1.0,4.0),"no") in
    let e6 = ((0.0,5.0,7.0),"no") in 
    let lst = [e1;e2;e3;e4;e5;e6] in 
    let lst1 = [e2;e1;e3;e5;e6;e4] in
    let t = build_balanced lst 0 empty in 
    let t2 = build_balanced lst 0 empty in 
    assert(is_balanced(t)); 
    assert(is_balanced(t2));
    ()
       
  let test_nearest_neighbor () =
    let e1 = ((3.0,1.0,4.0),"no") in 
    let e2 = ((2.0,3.0,7.0),"no") in
    let e3 = ((4.0,3.0,4.0),"no") in
    let e4 = ((2.0,4.0,5.0),"no") in
    let e5 = ((6.0,1.0,4.0),"no") in
    let e6 = ((0.0,5.0,7.0),"no") in 
    let test = ((0.1,5.0,7.0),"hell yeah") in 
    let test2 = ((6.0,1.1,4.0),"yup") in 
    let test3 = ((2.0,4.0,5.1),"no") in
    let test4 =((6.1,1.0,4.0), "plz") in 
    let test5 = ((2.1,3.0,7.0),"no") in 
    let lst = [e1;e2;e3;e4;e5;e6] in 
    let t = build_balanced lst 0 empty in 
    assert(nearest_neighbor test t = (e6, distance test e6));
    assert(nearest_neighbor test4 t = (e5, distance test4 e5));
    assert(nearest_neighbor test5 t = (e2, distance test5 e2)); 
    assert(nearest_neighbor test2 t = (e5, distance test2 e5));
    assert(nearest_neighbor test3 t = (e4, distance test3 e4));
    ()
  
  (* These tests can only be run when there's no abstraction, 
   * so in order to run them I implemented the COMPARABLE module within
   * the KDTree module, making it not a functor. When they
   * ran, the insert function worked completely. we ended up
   * not needing the insert function but the tests are still here *) 
  let test_insert ()  = 
    (*
    let e1 = ((3.0,1.0,4.0),"no") in 
    let e2 = ((2.0,3.0,7.0),"no") in
    let e3 = ((4.0,3.0,4.0),"no") in
    let e4 = ((2.0,4.0,5.0),"no") in
    let e5 = ((6.0,1.0,4.0),"no") in
    let t1 = insert e1 empty in 
    let t2 = insert e2 t1 in 
    let t3 = insert e3 t2 in 
    let t4 = insert e4 t3 in 
    let t5 = insert e5 t4 in 
    assert(t1 = Branch(Leaf,e1,Leaf));
    assert(t2 = Branch(Branch(Leaf,[e2],Leaf),[e1],Leaf));
    assert(t3 = Branch(Branch(Leaf,[e2],Leaf),[e1],Branch(Leaf,[e3],Leaf)));
    assert(t4 = Branch(Branch(Leaf,[e2],Branch(Leaf,[e4],Leaf)),[e1],
                Branch(Leaf,[e3],Leaf)));
    assert(t5 = Branch(Branch(Leaf,[e2],Branch(Leaf,[e4],Leaf)),[e1],
                Branch(Branch(Leaf,[e5],Leaf),[e3],Leaf)));
    *) 
    ()

  let run_tests () =
    test_build_balanced();
    test_nearest_neighbor();
    test_insert (); 
    ()

end

KDTree.run_tests();;
