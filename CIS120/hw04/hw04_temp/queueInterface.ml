(* Do NOT modify this file. *)

(******************************************************************************)
(* ABSTRACT QUEUE INTERFACE                                                   *)
(******************************************************************************)

(* This module signature defines the values associated with the
 * abstract type `'a queue`. Any module struct that implements the `QUEUE`
 * interface must contain all of the values defined in this interface;
 * otherwise it won't compile.
 *
 * You should read through this interface to understand the behaviors
 * it specifies before moving on to writing test cases in the file
 * qTest.ml. *)

module type QUEUE = sig

  (* The "abstract type" 'a queue.
   * 
   * In this assingment you will implement two QUEUE structs:
   *   - the first utilizes mutable state to build an imperatively linked data
   *     structure in the heap (LinkedQueue).
   *   - the second utilizes a simple recursive structure--a list (SimpleQueue).
   *
   * You will notice that LinkedQueue builds the queue 'imperatively' by
   * directly manipulating references, whereas SimpleQueue builds the queue
   * 'declaratively' by building a "bigger" queue out of a "smaller" one! *)  
  type 'a queue

  (* `valid q` check whether q satisfies the queue implementation invariants. *)
  val valid : 'a queue -> bool

  (* Create an empty queue *)
  val create : unit -> 'a queue

  (* `is_empty q` is true exactly when q has no elements *)
  val is_empty : 'a queue -> bool

  (* `enq x q` mutates q such that x is now the tail. *)
  val enq : 'a -> 'a queue -> unit

  (* `deq q` mutates q such that the head is removed and its
   * value is returned if it exists (i.e., the queue is not empty) *)
  val deq : 'a queue -> 'a

  (* `to_list q` returns a list of all the elements of q in the order they
   * appear in the queue. *)
  val to_list : 'a queue -> 'a list

  (* `length q` returns the length of q (number of elements) *)
  val length : 'a queue -> int

  (* `print q` prints the contents of q *)
  val print : 'a queue -> ('a -> string) -> unit

  (* `from_list l` returns a queue containing all the elements of the list l
   * in the order they appear in the list. *)
  val from_list : 'a list -> 'a queue
  
  (* `contains x q` returns true exactly when x appears somewhere in q *)
  val contains : 'a -> 'a queue -> bool

  (* `truncate x q` mutates q such that q is truncated after the first
   * occurrence of x. (i.e. it removes all elements
   * from the queue that follow the first occurrence of the given
   * element.) *)
  val truncate : 'a -> 'a queue -> unit

  (* `delete x q` mutates q such that all occurences of the value x in q are
   * deleted *)
  val delete : 'a -> 'a queue -> unit
  
  (* Kudos operation *)
  val has_cycle : 'a queue -> bool
  
  (* For testing and debugging *)
  val debug_name : string 
end

