(******************************************************************************)
(* PROBLEM 5: WRITING TEST CASES                                              *)
(******************************************************************************)

(* `SetTest` is a reuseable module that we'll use to test other modules that
 * conform to the `SET` interface. When `SetTest` is instantiated with a
 * particular implementation, it will run all of its test cases against the
 * set type defined in that implementation.  This means that the _same_ tests
 * can be used for both the OLSet and BSTSet implementations (and that both
 * implementations should behave the same for these tests!).
 *
 * Read through the module, and then write your test cases in the space
 * provided below. Your TAs will be grading the completeness of your tests. *)

module SetTest (SetImpl: SetInterface.SET) = struct
  ;; open SetImpl

  (* We'll redefine the `run_test` and `run_failing_test` functions so that
   * they prepend the name of the set we're testing to the test description. *)

  let run_test desc = Assert.run_test (debug_name ^ ": " ^ desc)
  let run_failing_test desc = Assert.run_failing_test (debug_name ^ ": " ^ desc)

  ;; print_endline ("\n--- Running tests for " ^ debug_name ^ " ---")

  (* We'll write all our test cases for the `SET` abstract type below.
   * Here are a few to help get you started. *)

  let test () : bool =
    is_empty empty
  ;; run_test "empty set is empty" test

  let test () : bool =
    let s = set_of_list [1; 2; 3] in
    not (is_empty s)
  ;; run_test "{1, 2, 3} is not empty" test



  (* Now, it's your turn! Make sure to comprehensively test all the other
   * functions defined in the `SET` interface. It will probably be helpful to
   * have the file setInterface.ml open as you work.  Your tests should 
   * stress the abstract properties of what it means to be a set, as well
   * as the relationships among the operations provided by the SET interface.
   *
   * Your TAs will be manually grading the completeness of your test cases. *)

  (* ---------- Write your own test cases below. ---------- *)

let test () : bool =
  is_empty (remove 5 (set_of_list [5]))
;; run_test "removing the only element produces empty set" test

let test () : bool =
    member 5 (add 5 empty)
  ;; run_test "once you add something it is a member" test

let test () : bool =
    let s = set_of_list [] in
    is_empty s
  ;; run_test "{} is empty" test


let test () : bool =
    add 5 empty = add 5 (set_of_list [5])
  ;; run_test "no duplicates" test

let test () : bool =
    is_empty (remove 5 (add 5 empty))
  ;; run_test "removing an added element has no effect" test

let test () : bool =
    remove 5 empty=empty
  ;; run_test "remove checks for membership" test

let test () : bool =
    size empty = size (set_of_list [1;2;3]) - 3
  ;; run_test "size of empty is 0 ; size of  {1,2,3} is 3" test

let test () : bool =
    equals empty empty
  ;; run_test "empty sets are equal" test

let test () : bool =
    equals (add 7 (add 6 (add 5 empty))) (add 7 (add 5 (add 6 empty)))
  ;; run_test "sets are unordered" test

let test () : bool =
    set_of_list (list_of_set (add 99 (add 5 empty))) = set_of_list [5;99]
  ;; run_test "set of list and list of set are inverse operations" test


  (* ---------- Write your own test cases above. ---------- *)

end

(* We now instantiate the tests so they are executed for OLSet and BSTSet.
 * Don't modify anything below this line. *)

module TestOLSet = SetTest(ListSet.OLSet)
;; print_newline ()

module TestBSTSet = SetTest(TreeSet.BSTSet)
;; print_newline ()

