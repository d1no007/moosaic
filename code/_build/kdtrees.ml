
open Color 


type order = Equal | Less | Greater
type point = Rgb.t
type image = string ref  

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

  (* Builds a balanced tree so that a nearest neighbor 
   * search ban be run in optimal time *)
  val build_balanced: elt list -> int -> tree -> tree 

  (* Search a KD tree for the given value and returns
  * the nearest neighbor *) 
  val nearest_neighbor : elt -> tree -> elt

  (* Run invariant checks on the implementation of this KD tree.
   * May raise Assert_failure exception -- examples of these tests 
   * can be found in kdtest.ml *)
  val run_tests : unit -> unit
end


module type COMPARABLE =
sig
  (* the type of element *) 
  type t
  
  (* a method for comparing elements *) 
  val compare : int -> t -> t -> order
  
  (* a method of finding the distance between two elements *)
  val distance : t -> t -> int 
  
  (* a method of finding the distance between an elt and the 
   * corresponding plane of a second elt *) 
  val distance_to_plane : int -> t -> t -> int 
end


(*a module for comparing the actual elements used in the KDTree*)
module PointCompare : COMPARABLE with type t= point*image =
struct
 
  (* the data in our basis of images is represented as a point --
   * the average color vector of the image, and a string that 
   * is a name of that image *) 
  type t = point * image 
  
  (* helper function to compare the r values *) 
  let compareR x y = 
    let (p1,_),(p2,_) = x,y in 
     if p1.r > p2.r then Greater 
     else if p1.r < p2.r then Less 
     else Equal 

  (* helper function to compare the g values *) 
  let compareG x y = 
    let (p1,_),(p2,_) = x,y in 
     if p1.g > p2.g then Greater 
     else if p1.g < p2.g then Less 
     else Equal 
  
  (* helper function to compare the b values *) 
  let compareB x y = 
    let (p1,_),(p2,_) = x,y in 
     if p1.b > p2.b then Greater 
     else if p1.b < p2.b then Less 
     else Equal 
  
  (* compares the two points given a level in the tree 
   * (a dimmension to compare with) *)
  let compare i x y = 
    match (i mod 3) with 
    | 0 -> compareR x y 
    | 1 -> compareG x y 
    | 2 -> compareB x y 
    | _ -> failwith "shouldn't return any other value"
      
 (*calculates the distance between two points*)
  let distance (e1: t) (e2: t) =
    let (p1,_),(p2,_) = e1,e2 in 
    Rgb.square_distance p1 p2 

  (*calculates the distance between a point and the 
   * hyperplane--used to see if other side of tree
   * needs to be traversed as well *)         
  let distance_to_plane (c: int) e1 e2 = 
    match (c mod 3) with 
    | 0 -> let (p,x) = e2 in distance e1 ({r = p.r; g = 0; b = 0},x) 
    | 1 -> let (p,x) = e2 in distance e1 ({r = 0; g = p.g; b = 0},x)
    | 2 -> let (p,x) = e2 in distance e1 ({r = 0; g = 0; b = p.b},x)  
    | _ -> failwith "shouldn't be any other value!" 
 
end



module KDTree(C : COMPARABLE) : KDTREE with type elt = C.t =
struct
  exception EmptyTree
  exception NodeNotFound

  (* Grab the type of the tree from the module C that's passed in *)
  type elt = C.t 

  (* One possible type for a tree *)
  type tree = Leaf | Branch of tree * elt * tree

  (* Representation of the empty tree *)
  let empty = Leaf
  
 (* function for determining  max of an int to help with
  * is_balanced function for testing *)
  let max x y = if x > y then x else y 

  
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

  (* funtion to determine min depth of a tree *)
  let min_depth (t: tree) : int = 
    let rec min_dep (m: int) (lst: (tree*int) list) = 
    match lst with 
    | [] -> m 
    | (Leaf,c) :: tl -> if c < m then min_dep c tl else min_dep m tl 
    | (Branch(l,lst,r),c)::tl -> min_dep m ((l, c+1)::(r,c+1)::tl)
  in min_dep (max_depth t) [(t,0)]


  (* function to check if tree is balanced - checks to see if 
   * min_depth and max_depth are one apart or equal. helps 
   * for testing purposes *)
  let is_balanced (t: tree) : bool = 
    if abs(max_depth t - min_depth t) <= 1 then true
    else false 

  (* function that will help sort by designated dimmension -- 
   * returns 0, 1, or -1 so it can be used with the List.sort 
   * function *)
  let sort (x: elt) (y: elt) (c: int): int = 
    match C.compare c x y with 
    | Equal -> 0 
    | Greater -> 1 
    | Less -> -1 

  (* sorts all the elements in the tree by given dimmension *)
  let sort_list (lst: elt list) (c: int) : elt list = 
    List.sort (fun x y -> sort x y c) lst 
  
  (*find what index the median of a sorted list is at*) 
  let index (lst: elt list) : int =
      let length = List.length lst in 
        if length mod 2 = 0 then 
            length / 2
        else (length - 1) / 2 
    
  (* finds the median of a sorted list *)
  let find_median (lst: elt list) : elt  =  
       List.nth lst (index lst)
      
  
  (* takes in a list, and divides it in half at the median. 
   * used for the build_balanced function so that the 
   * tree is built smart *) 
  let rec divide_lists lst left right middle c median = 
    match lst with 
    | [] -> (left, right) 
    | hd :: tl -> if c < median then 
                    divide_lists tl (hd :: left) right middle (c+1) median 
                  else if c > median then 
                    divide_lists tl left (hd :: right) middle (c+1) median
                  else 
                    divide_lists tl left right (hd :: middle) (c+1) median 
                    
  (* builds a balanced tree -- elements are sorted by their first 
   * component, the median is used as the root of the tree, and then
   * the remaining elements are recursively subdivided into children
   * of the root node *)
  let rec build_balanced (lst: elt list) (c: int) (t: tree) : tree = 
    match lst with 
    | [] -> t
    | _ -> let sorted_lst = sort_list lst c in 
           let new_node = find_median sorted_lst in 
           let (left_list, right_list) = 
            divide_lists (sorted_lst) [] [] [] 0 (index lst) 
           in Branch((build_balanced left_list (c+1) empty), new_node, (build_balanced right_list (c+1) empty))
           
       
  (* Nearest neighbor function for KD trees -- takes in a type elt and a tree and 
  * returns the elt closest to the given elt in the tree *)
  let nearest_neighbor (x : elt) (t : tree) : elt  =
    let rec traverse (o: elt) (current_best: elt) (k: tree) (d: int) (c: int): elt =
        match k with 
        | Leaf -> current_best
        | Branch (l, hd, r) -> 
            let (current_best, d) = if C.distance hd o < d then (hd, C.distance hd o) else (current_best, d) in
            match C.compare c o hd with
                | Less -> 
                    let current_best = traverse o current_best l d (c+1) in
                    if C.distance_to_plane c o hd <= d then
                    traverse o current_best r d (c+1) else current_best
                | Greater | Equal -> 
                    let current_best = traverse o current_best r d (c+1) in
                    if C.distance_to_plane c o hd <= d then 
                    traverse o current_best l d (c+1) else current_best in 
  match t with 
  | Leaf -> failwith "there are no nodes in this tree"
  | Branch(_, hd, _) -> traverse x hd t (C.distance hd x) 0 
            
  (* because of abstraction, these tests weren't implemented in this file. 
   * in the file kdtest.ml, you can run these tests and check for 
   * assert failures *) 
  let test_balance () = 
    ()
    
  let test_nearest_neighbor () =
    ()
 
  let test_insert ()  = 
    ()

  let run_tests () =
    ()

end

module PointTree = KDTree(PointCompare);;
