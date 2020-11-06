;; open Assert

(******************************************************************************)
(* Problem 1: Options  ********************************************************)
(******************************************************************************)

(* OCaml provides a  'a option  type, which allows algorithms to indicate
 * that they were unable to come up with a more useful value to return.
 *
 * type 'a option = None | Some of 'a
 *
 * For example, consider this version of assoc for lists:
 *
 *    let rec assoc : (key: 'k) (l : ('k * 'v) list) : 'v =
 *      begin match l with
 *      | [] -> failwith "Not_found"
 *      | (k2,v):: l -> if key = k2 then v else assoc k l
 *      end
 *
 * Here the return type 'v is sort of a lie, isn't it?  If the key isn't
 * found in the list, assoc hits a failwith instead of returning an
 * actual value.
 *
 * Use options to re-implement assoc with a more truthful type: *)
let rec assoc (k: 'k) (l: ('k * 'v) list) : 'v option =
failwith "Missing implementation for assoc"

let test () : bool =
  assoc 1 [(1, 2)] = Some 2
;; run_test "assoc key exists" test

let test () : bool =
  assoc 3 [(1, 2)] = None
;; run_test "assoc key doesn't exist" test


(* Write a function that converts an optional optional value into just an
 * optional value. *)
let join_option (x: 'a option option) : 'a option =
failwith "join_option: unimplemented"

let test () : bool =
  join_option (Some (Some 3)) = Some 3
;; run_test "join_option Some option" test

(* Write a function that takes a list of optional values and returns a list
 * containing all of the values that are present. *)
let rec cat_option (l: 'a option list) : 'a list =
  failwith "cat_option: unimplemented"

let test () : bool =
  cat_option [ Some 1; None; Some 2; Some 0; None; None] = [1;2;0]
;; run_test "cat_option list contains Some and None options" test

(* Write a function that transforms a list using a partial function. *)
let rec partial_transform (f: 'a -> 'b option) (l: 'a list) : 'b list =
failwith "partial_transform: unimplemented"

let test () : bool =
  let f = fun x -> if x > 0 then Some (x * x) else None in
  partial_transform f [0; -1; 2; -3] = [4]
;; run_test "partial_transform positive squaring" test


(******************************************************************************)
(* Problem 2: Mutability, Aliasing and Refs  **********************************)
(******************************************************************************)

(* Implement a function `iter` that calls a side-effecting function on each
 * element of a list. You should not use any List library functions for this. *)
let rec iter (f: 'a -> unit) (l: 'a list) : unit =
  failwith "iter: unimplemented"

(* Here's a test for `iter`, using it to increment each mutable
 * reference in an int ref list. *)

let test () : bool =
  let l = [ref 0; ref 1; ref 2] in
  iter (fun r -> r.contents <- 1 + r.contents) l;
  l = [ref 1; ref 2; ref 3]
;; run_test "iter non-empty list" test

(* Let's explore references in more detail, starting with the basics:
 *
 * The notation 'a ref is just shorthand for a mutable record with one
 * field called 'contents':
 *
 * type 'a ref = { mutable contents : 'a }
 *
 * '=' is used to check for structural equality, whereas
 * '==' is used to check for reference equality. *)

(* Write a function to increment the contents of a reference cell containing an
 * int. The increment function should return the old value of the reference. *)
let ref_incr (r : int ref) : int =
failwith "ref_incr: unimplemented"

let test () : bool =
  let r = { contents = 0 } in
  ref_incr r == 0 && ref_incr r == 1 && r.contents == 2
;; run_test "ref_incr incrementing twice" test


(* Write a function to swap the contents of two reference cells. *)
let swap (r1: 'a ref) (r2: 'a ref) : unit =
  failwith "swap: unimplemented"

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = { contents = 6 } in
  let _ = swap r1 r2 in
  (6, 5) = (r1.contents, r2.contents)
;; run_test "swap refs with different contents" test

(* One troubling issue with mutable state is *aliasing* between
 * mutable bindings. *)

(* Write a function that determines if two ref cells are aliased.  Use
 * reference equality. *)
let refs_aliased (r1: 'a ref) (r2: 'a ref) : bool =
  failwith "aliased: unimplemented"

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = { contents = 5 } in
  let b = refs_aliased r1 r2 in
  (false, true, true) = (b, r1.contents = 5, r2.contents = 5)
;; run_test "refs_aliased different records with same value not aliased" test

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = r1 in
  let b = refs_aliased r1 r2 in
  (true, true, true) = (b, r1.contents = 5, r2.contents = 5)
;; run_test "refs_aliased aliased records are aliased" test

(***** KUDOS Problem (int_refs_aliased) *********)

(* Now write another function to test whether two int refs are aliased without
 * using reference equality. (i.e. do NOT use OCaml's '==' or '!=' operators
 * for this problem.) At the end of the function, both refs *must* be in the
 * same state they started in!  *)
let int_refs_aliased (r1: int ref) (r2: int ref) : bool =
  failwith "int_refs_aliased: unimplemented"

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = { contents = 5 } in
  let b = int_refs_aliased r1 r2 in
  (false, true, true) = (b, r1.contents = 5, r2.contents = 5)
;; run_test "int_refs_aliased dif. records with same value are not aliased" test

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = r1 in
  let b = int_refs_aliased r1 r2 in
  (true, true, true) = (b, r1.contents = 5, r2.contents = 5)
;; run_test "int_refs_aliased aliased records are aliased" test

(******************************************************************************)
(* Problem 3: Equality and Aliases ********************************************)
(******************************************************************************)

(* Write a function that, given a value x and a list of reference
 * values, determines whether any of the elements in the list is an
 * alias of x. (HINT: use reference equality!)  Do NOT use the 'refs_aliased'
 * function to help with this problem. *)
let rec contains_alias (x: 'a) (l: 'a list) : bool =
  failwith "contains_alias unimplemented"

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = { contents = 5 } in
  contains_alias r1 [r2] = false
;; run_test "contains_alias singleton no alias" test

let test () : bool =
  let r1 = { contents = 5 } in
  let r2 = { contents = 5 } in
  contains_alias r1 [r2; r1]
;; run_test "contains_alias multi-element list alias exists" test

(* Let's briefly review equality using the ASM!
 * 
 * Note:
 *   - 2 values are *structurally* equal when the value they point to on the
 *     heap *look* the same.
 *   - 2 values are *referentially* equal when they *point to* the same value
 *     on the heap
 *
 * Complete the test result below with a list of boolean values in
 * such a way that the test passes. Make sure you understand why each
 * comparison returns true or false before continuing to the next part
 * of the homework assignment. Hint: draw the ASM! *)
let equality_test_results () : bool list =
  failwith "add a list of results for the following equalities."

let test () : bool =
  let r = { contents = 5 } in
  let o = Some r in
  [ r = r;
    r == r;
    r = { contents = 5 };
    r == { contents = 5 };
    { contents = 5 } = { contents = 5 };
    { contents = 5 } == { contents = 5 };
    r.contents = r.contents;
    r.contents == r.contents;
    Some r = Some r;
    Some r == Some r;
    Some r = o;
    Some r == o;
    o = o;
    o == o;
    contains_alias o [o];
    contains_alias (Some r) [Some r]
  ] = equality_test_results ()
;; run_test "interactions between == and options" test

(* Now write a function that determines whether a given value x is
 * aliased within a list of optional reference values.  NOTE: Assume
 * that 'a will only be instantiated with record type containing a
 * mutable field. *)
let rec contains_alias_option (x: 'a) (l: 'a option list) : bool =
  failwith "contains_alias_option: unimplemented"

let test () : bool =
  let r = { contents = 5 } in
  let o1 = Some r in
  contains_alias_option r [o1]
;; run_test "contains_option_alias singleton contains alias" test

let test () : bool =
  let r = { contents = 5 } in
  let o2 = Some { contents = 6 } in
  not (contains_alias_option r [o2])
;; run_test "contains_option_alias singleton does not contain alias" test
