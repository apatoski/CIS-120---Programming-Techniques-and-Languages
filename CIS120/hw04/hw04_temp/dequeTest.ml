(******************************************************************************)
(* PROBLEM 7: WRITING TEST CASES                                              *)
(******************************************************************************)

;; open Assert
;; open Deque

(* `DequeTest` is used to test the deque implementation from deque.ml. 
 *
 * Read through the module, then write your test cases in the space
 * provided below.  Your TAs will be grading the completeness of your
 * tests. *)

;; print_endline ("\n--- Running tests for Deque ---")

(* We'll write all our test cases for deque below.
 * Here is a test to help get you started. *)

let test () : bool =
  is_empty (create ())
;; run_test "is_empty: call on empty returns true" test

(* Now, it's your turn! Make sure to comprehensively test all the other
 * functions you implemented in deque.ml. It will probably be helpful to
 * have the files deque.ml/mli open as you work.
 *
 * We provide many test cases for you, so your job here is to finish writing
 * tests for `remove_head`, `remove_tail`, `delete_last`, `delete_first`, and
 * `reverse`.
 *
 * Your TAs will be manually grading the completeness of your test cases. *)

(* ---------- Write your own test cases below. ---------- *)
(* INSERT_HEAD TESTS *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_head into empty" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  valid d && peek_head d = 2 && peek_tail d = 1
;; run_test "insert_head into singleton" test

let test () : bool =
  let d = create () in
  insert_head 1 d;
  insert_head 2 d;
  insert_head 3 d;
  valid d && peek_head d = 3 && peek_tail d = 1
;; run_test "insert_head into non-empty, multi-element" test

(* INSERT_TAIL TESTS *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  valid d && peek_head d = 1 && peek_tail d = 1
;; run_test "insert_tail into empty" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  valid d && peek_head d = 1 && peek_tail d = 2
;; run_test "insert_tail into singleton" test

let test () : bool =
  let d = create () in
  insert_tail 1 d;
  insert_tail 2 d;
  insert_tail 3 d;
  valid d && peek_head d = 1 && peek_tail d = 3
;; run_test "insert_tail into non-empty, multi-element" test

(* REMOVE_HEAD *)
let test () : bool =
  let d = create () in
  insert_head 1 d;
  remove_head d = 1
;; run_test "remove_head singleton" test


(* REMOVE_TAIL *)
let test () : bool =
  let d = create () in
  insert_tail 1 d;
  remove_tail d = 1
;; run_test "remove_tail singleton" test


(* DELETE_LAST *)


(* DELETE_FIRST *)


(* REVERSE *)


(* ---------- Write your own test cases above. ---------- *)
