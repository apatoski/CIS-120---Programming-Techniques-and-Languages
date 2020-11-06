(******************************************************************************)
(* Problem 5: Simple Queues ***************************************************)
(******************************************************************************)

;; open Assert

(* This problem takes a very simple approach to implementing the queue data
 * structure.  We define the type 'a queue to be a 'a list ref -- that is,
 * a queue is just a mutable cell containing a list. *)

module SimpleQueue : QueueInterface.QUEUE = struct

  type 'a queue = 'a list ref

  (* All SimpleQueues are valid *)
  let valid (q: 'a queue) : bool =
    true
  
  (* create an empty SimpleQueue *)
  let create () : 'a queue =
    ref []
  
  (* determine whether a queue is empty *)
  let is_empty (q: 'a queue) : bool =
    !q = []
  
  (* add an element to the tail of a queue *)
  let enq (elt: 'a) (q: 'a queue) : unit =
    q.tail <- elt
  
  (* remove an element from the head of the queue *)
  let deq (q: 'a queue) : 'a =
    begin match !q with
    | [] -> failwith "deq called on empty queue"
    | hd :: tl -> q := tl; hd
    end
  
  (* Retrieve the list of values stored in the queue in the order they appear.
   * Be sure to leverage the implementation invariant of this queue type! *)
  let to_list (q: 'a queue) : 'a list =
    !q
  
  (* Get the length of a queue. You may use List library functions for this
   * function. *)
  let length (q: 'a queue) : int =
    List.length !q
  
  (* Print out the contents of a queue *)
  let rec print (q: 'a queue) (string_of_element: 'a -> string) : unit =
      print_endline "--- queue contents ---";
      List.iter (fun x -> print_endline (string_of_element x)) !q;
      print_endline "--- end of queue ---"
  
  (* Convert a list into a queue; be sure to leverage the implementation
   * invariant of this queue type! *)
  let from_list (l: 'a list) : 'a queue =
    failwith "to_list: unimplemented"
  
  (* Determine whether a given element is in the queue. Test the elements for
   * equivalence using referential equality (==) *)
  let contains (elt: 'a) (q: 'a queue) : bool =
    List.fold_right (fun v acc -> v == elt || acc) !q false
  
  (* Truncate a queue after the first occurrence of a specified element. (i.e.
   * it removes all elements from the queue that follow the first occurrence of
   * the given element.) Remember to use referential equality. *)
  let truncate (elt: 'a) (q: 'a queue) : unit =
    failwith "truncate: unimplemented"
  
  (* Delete all instances of the value elt from the queue q. *)
  let delete (elt: 'a) (q: 'a queue) : unit =
    q := List.fold_right (fun v acc -> if v = elt then acc else v :: acc) !q []
  
  (* A SimpleQueue can't have a cycle. It's a list! *)
  let has_cycle (q: 'a queue) : bool =
    false
  
  (* DO NOT MODIFY *)
  let debug_name = "SimpleQueue"
end
