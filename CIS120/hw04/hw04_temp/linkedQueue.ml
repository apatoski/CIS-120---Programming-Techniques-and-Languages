(******************************************************************************)
(* Problem 6: Linked Queues ***************************************************)
(******************************************************************************)

;; open Assert
;; open Imp

(* This problem extends the queue example that we covered in class
 * with a few additional exercises. Note that there is some new code
 * here too that you will also need to understand in preparation for
 * the rest of the assignment.  *)

(* Here are mutable queues, as defined in class. *)

module LinkedQueue : QueueInterface.QUEUE = struct
  
  type 'a qnode = { v: 'a;
                    mutable next: 'a qnode option }

  type 'a queue = { mutable head: 'a qnode option;
                     mutable tail: 'a qnode option }
  
  (* INVARIANT:
   * - q.head and q.tail are either both None, or
   * - q.head and q.tail both point to Some nodes, and
   *   - q.tail is reachable by following 'next' pointers
   *     from q.head
   *   - q.tail's next pointer is None
   *
   * Your job will be to preserve this invariant in all of the functions
   * of the queue module (unless we specifically tell you not to.)
   *
   * Note that this invariant implies that there are no cycles in the
   * queue, meaning that you will never encounter the same node twice if
   * you were to traverse the queue via 'next' pointers starting from
   * the head. Queues that do not satisfy this invariant could have
   * cycles, such as the two queues below.  *)
  let cyclic_queue_1 () : int queue =
    let n = { v = 1; next = None } in
    n.next <- Some n;
    { head = Some n; tail = Some n }

  let cyclic_queue_12 () : int queue =
    let n2 = { v = 2; next = None } in
    let n1 = { v = 1; next = Some n2 } in
    n2.next <- Some n2;
    { head = Some n1; tail = Some n2 }  

  (* To check that a given queue is valid (i.e. that it satisfies the
   * invariant) we must write a function that walks down the list
   * looking for the tail.  This function must keep track of which nodes
   * have been traversed and return None if a cycle is ever encountered.
   *
   * The function get_tail (below) returns (Some n) where n is the tail
   * of the queue found by traversing next pointers from qn until we
   * find the last node (i.e. a node whose 'next' pointer is None).
   *
   * If we ever visit a node that has already been seen (indicating
   * that there is a cycle in the queue), the answer is None.
   *
   * Note that this function uses contains_alias, defined in imp.ml.*)
  let rec get_tail (qn: 'a qnode) : 'a qnode option =
    let rec loop (curr: 'a qnode) (seen: 'a qnode list) : 'a qnode option =
      begin match curr.next with
      | None -> Some curr
      | Some n ->
          if contains_alias n seen
          then None
          else loop n (curr :: seen)
      end
  in loop qn []
  
  (* The following function (valid) checks whether a particular queue
   * value satisfies the queue implementation invariant.
   *
   * Each of the functions below check the validity of their arguments
   * as an aid to debugging.  In practice, doing such costly
   * validity checking ruins the performance of the queue operations, so
   * the checks would be removed once the code is debugged.  *)
  let valid (q: 'a queue) : bool =
    begin match q.head, q.tail with
    | None, None -> true
    | Some qh, Some qt ->
        begin match get_tail qh with
        | Some n -> qt == n (* the tail points to the last node *)
        | None -> false
        end
    | _, _ ->  false
    end
  
  (******  QUEUE functions from class ******)
  
  (* create an empty queue *)
  let create () : 'a queue =
    { head = None;
      tail = None }
  
  (* determine whether a queue is empty *)
  let is_empty (q: 'a queue) : bool =
    q.head = None
  
  (* add an element to the tail of a queue *)
  let enq (elt: 'a) (q: 'a queue) : unit =
    if not (valid q) then failwith "enq: given invalid queue";
    let newnode = Some { v = elt; next = None } in
    begin match q.tail with
    | None ->
        (* Note that the invariant tells us that q.head is also None *)
        q.head <- newnode;
        q.tail <- newnode
    | Some n ->
        n.next <- newnode;
        q.tail <- newnode
    end
  
  (* remove an element from the head of the queue *)
  let deq (q: 'a queue) : 'a =
    if not (valid q) then failwith "deq: given invalid queue";
    begin match q.head with
    | None ->
        failwith "deq called on empty queue"
    | Some n ->
        q.head <- n.next;
        if n.next = None then q.tail <- None;
        n.v
    end
  
  (* Retrieve the list of values stored in the queue, ordered from
   * head to tail. *)
  let to_list (q: 'a queue) : 'a list =
    if not (valid q) then failwith "to_list: given invalid queue";
    let rec loop (no: 'a qnode option) (l: 'a list) : 'a list =
      begin match no with
      | None -> List.rev l
      | Some n -> loop n.next (n.v :: l)
      end
    in loop q.head []
  
  (* get the length of a queue *)
  let length (q: 'a queue) : int =
    if not (valid q) then failwith "length: given invalid queue";
    let rec loop (no: 'a qnode option) (len: int) : int =
      begin match no with
      | None -> len
      | Some n -> loop n.next (len + 1)
      end
    in loop q.head 0
  
  (* print out the contents of a queue *)
  let rec print (q: 'a queue) (string_of_element: 'a -> string) : unit =
    if not (valid q) then failwith "print: given invalid queue";
    let rec loop (no: 'a qnode option) : unit =
      begin match no with
      | None -> ()
      | Some n -> print_endline (string_of_element n.v); loop n.next
      end
    in
      print_endline "--- queue contents ---";
      loop q.head;
      print_endline "--- end of queue -----"
  
  (**********************************************)
  (******  QUEUE problems ***********************)
  
  (* Now it is your turn: implement a function that converts a list
   * [v1; v2; ...; vn] into a queue with v1 is at the head and vn at
   * the tail. *)
  let from_list (l: 'a list) : 'a queue =
    failwith "from_list: unimplemented"
  
  
  (* For the rest of this task, do NOT use to_list to convert
   * any given queues into lists. *)

  (* Implement a function that determines whether a given element is in
   * the queue. Test the elements for equivalence using referential
   * equality (==) . *)
  let contains (elt: 'a) (q: 'a queue) : bool =
    if not (valid q) then failwith "contains: given invalid queue";
    failwith "contains: unimplemented"
  
  (* Implement a function that truncates a queue after the first
   * occurrence of a specified element. (i.e. it removes all elements
   * from the queue that follow the first occurrence of the given
   * element.) The function should mutate the queue in place.
   * In place means that you should not construct a new queue, but instead
   * should update the pointers within the given queue. Remember to use
   * referential equality. *)
  let truncate (elt: 'a) (q: 'a queue) : unit =
    if not (valid q) then failwith "truncate: given invalid queue";
    failwith "truncate: unimplemented"
    
  (* Implement a function that deletes all nodes containing the value elt
   * from the queue q.  This function should mutate the queue in place.
   * In place means that you should not construct a new queue, but instead
   * should update the pointers within the given queue. Use structural
   * eqauality.
   *
   * Advice: Work out examples on paper first.
   *
   * Hints:
   *   - you will need to both use and maintain the queue invariants
   *
   *   - think about these cases:
   *       . what if the queue is empty?
   *       . what if the only element of the queue is the value to
   *         be deleted?
   *       . what if the first or last element of the queue
   *         has the value to be deleted?
   *
   *   - you may want to consider adding one or more helper functions
   *     that traverse the queue *)
  let delete (elt: 'a) (q: 'a queue) : unit =
    if not (valid q) then failwith "delete: given invalid queue";
    failwith "delete: unimplemented"
  
  
  (****************************************************************)
  (**** Queue Kudos problem  **************************************)
  (****************************************************************)
  
  (* The given valid queue function uses a helper function named
   * "contains_alias," which keeps track of seen nodes. This can use up a lot
   * of memory and be VERY inefficient when the queue is large.
   *
   * We can do better.  Without using contains_alias, given a queue, determine
   * whether or not it has a cycle.  This goes without saying, you may NOT keep
   * a list of nodes seen.
   *
   * REMEMBER: A cycle is when a node's next pointer points to a node
   * that has already been seen.  THIS MEANS THAT IF YOU WERE TO
   * TRAVERSE A QUEUE WITH A CYCLE NORMALLY, YOU WOULD RUN INTO AN
   * INFINITE LOOP. *)
  let has_cycle (q: 'a queue) : bool =
    failwith "has_cycle: unimplemented"
  
  (* DO NOT MODIFY *)
  let debug_name = "LinkedQueue"
end
