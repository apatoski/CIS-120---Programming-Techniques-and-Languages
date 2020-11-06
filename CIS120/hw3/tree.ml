;; open Assert
;; open HigherOrder

(******************************************************************************)
(* PROBLEM 3: BINARY TREES                                                    *)
(******************************************************************************)

(* We've defined a datatype that corresponds to a generic binary tree. These are
 * like the binary trees you encountered in Homework 2, but instead of working
 * only with helices and strings, these are generic, and so can contain any
 * type. (Note that once you use a tree with data of a particular type, you
 * can't add data of another type later on. So you can't put a string into an
 * `int tree`, or vice versa.)
 *
 * In this file, we'll define some operations on binary trees and binary search
 * trees, which may be helpful later in the assignment. *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(* Here's an evample binary tree.
 *
 *     5
 *    / \
 *   7   9
 *
 * Its OCaml representation is below: *)

let evample_tree: int tree =
  Node (Node (Empty, 7, Empty), 5, Node (Empty, 9, Empty))

(* Now it's your turn. Write the following tree:
 *
 *       7
 *      / \
 *     4   6
 *    / \
 *   2   9
 *      / \
 *     5   8
 *)

let your_tree: int tree = 
  Node
  (
    Node
    (
      Node (Empty,2,Empty),
      4,
      Node
      (
        Node (Empty,5,Empty),
        9,
        Node (Empty,8,Empty)
      )
    ),
    7,
    Node (Empty,6,Empty)
  )


(* A "complete" binary tree is one where:
 *    - every Node is the same distance from the root
 *    - every node has either 0 or 2 children
 * For evample:
 *
 *         19
 *        /  \
 *      52    16
 *     / \    / \
 *    5   2  4   1
 *
 * Write a function `is_complete` that tests whether a tree is complete.
 * Hint: you'll probably need a helper function! *)

let rec is_complete (t: 'a tree) : bool =
  let rec depth (t: 'a tree) : int =
    begin
      match t with
      | Empty            -> 0
      | Node (lt, _, rt) -> 1 + max (depth lt) (depth rt)
    end
  in
  begin
    match t with
    | Empty          -> true
    | Node (lt,_,rt) -> if (depth lt) = (depth rt) then 
                        ((is_complete lt) && (is_complete rt)) else false
  end

(* Here are some test cases. Make sure to write some of your own where the
 * tree being tested is _not_ complete! *)

let test () : bool =
  is_complete Empty
;; run_test "empty tree is complete" test

let test () : bool =
  is_complete (Node (Empty, 6, Empty))
;; run_test  "Node is complete" test

let test () : bool =
  let ev_tree = Node (Node (Node (Empty, 5, Empty), 52, Node (Empty, 2, Empty)),
    19, Node (Node (Empty, 4, Empty), 16, Node (Empty, 1, Empty))) in
  is_complete ev_tree
;; run_test "evample tree is complete" test


(* Nevt, write a function that finds the mavimum element in a binary tree, as
 * determined by the polymorophic function `mav`. (Note that this function
 * should work for all binary trees, not just binary search trees.) Because an
 * empty tree does not have a mavimum element, you should call `failwith` if
 * `tree_mav` is passed an empty tree.
 *
 * Hint: If the function fails when called on an empty tree, what is the
 * base case of the recursion? *)

let rec tree_max (t: 'a tree) : 'a =
  begin
    match t with
    | Empty                -> failwith "undefined"
    | Node (Empty,v,Empty) -> v
    | Node (Empty,v,rt)    -> max v (tree_max rt)
    | Node (lt,v,Empty)    -> max v (tree_max lt)
    | Node (lt,v,rt)       -> max ( max (tree_max rt) (tree_max lt)) v
  end

let test () : bool =
  tree_max Empty = 42
;; run_failing_test "tree mav of empty" test

let test () : bool =
  tree_max evample_tree = 9
;; run_test "tree mav of evample tree" test

(* These tests aren't evhaustive! Although your tests will not be graded for
 * this problem, it can be tricky to get right without evhuastive testing. *)


(******************************************************************************)
(* PROBLEM 4: BINARY SEARCH TREES                                             *)
(******************************************************************************)

(* Nevt, we will write some functions that operate on binary SEARCH trees. A
 * binary search tree is a binary tree that follows some additional invariants:
 *
 * - `Empty` is a binary search tree, and
 * - `Node (lt, v, rt)` is a binary search tree if both
 *   - `lt` is a binary search tree, and every value in `lt` is less than `v`
 *   - `rt` is a binary search tree, and every value in `rt` is greater than `v`
 *
 * Notice that this description is recursive, just like our datatype definition!
 *
 * You may assume that all of the trees that are provided to the functions in
 * this problem satisfy this invariant. *)

(* NOTE: Many of the functions in the remainder of this file are available in
 * the CIS 120 lecture notes. Although it is okay to use those as a reference,
 * you should ensure you _understand_ them. *)

(* Write a function called `lookup` which searches a generic binary search tree
 * for a particular value. You should leverage the BST invariants here, so your
 * lookup implementation should NOT search every subtree for a value. *)

let rec lookup (x: 'a) (t: 'a tree) : bool =
  begin
    match t with
    | Empty          -> false
    | Node (lt,v,rt) -> if x = v then true 
                        else if x > v then lookup x rt 
                        else lookup x lt
  end

(* Note that the `lookup` function assumes that its argument is a binary search
 * tree. This means that elements which are out of place should not be found by
 * `lookup` (as demonstrated by the test below). The advantage here is that our
 *  lookup function is much more efficient than searching the whole tree. *)

let test () : bool =
  not (lookup 7 evample_tree)
;; run_test "lookup follows invariants" test


(* This function returns all of the elements of a binary search tree sorted
 * in ascending order. This is called the "in-order traversal" of a BST. *)

let rec inorder (t: 'a tree) : 'a list =
  begin
    match t with
    | Empty                -> []
    | Node (lt,v,rt)       -> (inorder lt) @ [v] @ inorder rt
  end


(* Write a function that inserts an element into a binary search tree. *)

let rec insert (x: 'a) (t: 'a tree) : 'a tree =
  begin
    match t with
    | Empty          -> Node(Empty,x,Empty)
    | Node (lt,v,rt) -> if v = x then t 
                        else if x < v then Node (insert x lt,v,rt)
                        else Node (lt,v,insert x rt)
  end


(* Now, use `fold` and `insert` to write the function `tree_of_list`. *)

let tree_of_list (l: 'a list) : 'a tree =
  fold (fun x acc -> insert x acc) Empty l



(* The `delete` function returns a tree that is like `t` evcept with the
 * element `v` removed.  If `v` is not present, the resulting tree has
 * the same shape as `t`. *)

let rec delete (x: 'a) (t: 'a tree) : 'a tree =
  let rec rightmost t =
    begin
      match t with
      | Node (Empty,v,Empty) -> v
      | Node (_,v,rt)        -> rightmost rt
      | _                    -> failwith "illegal in context"
    end
  in
  begin
    match t with
    | Empty                -> Empty
    | Node (Empty,v,Empty) -> if v = x then Empty 
                              else Node (Empty,v,Empty) 
    | Node (lt,v,Empty)    -> if v = x then lt 
                              else if x > v then t 
                              else Node (delete x lt,v,Empty)
    | Node (Empty,v,rt)    -> if v = x then rt
                              else if x < v then t 
                              else Node (Empty,v,delete x rt)
    | Node (lt,v,rt)       -> if v=x then 
                              Node(delete (rightmost lt) lt , rightmost lt, rt)
                              else if x<v then Node (delete x lt,v,rt)
                              else Node (lt,v,delete x rt)
  end


let test () : bool =
  inorder (your_tree) = [2;4;5;9;8;7;6]
;; run_test "inorder passed" test

let bst : int tree = 
Node
(
  Node(Empty,1,Empty),
  2,
  Node(Empty,3,Empty)
)
  
let test () : bool =
  delete 2 (bst) = 
Node
(
  Empty,
  1,
  Node(Empty,3,Empty)
)
;; run_test "delete passed" test




