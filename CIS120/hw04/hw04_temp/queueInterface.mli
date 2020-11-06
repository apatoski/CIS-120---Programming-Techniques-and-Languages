(* You do not need to look at this file -- it is generated
   automatically by Eclipse, but it plays no role in this
   assignment. *)

module type QUEUE =
  sig
    type 'a queue
    val valid : 'a queue -> bool
    val create : unit -> 'a queue
    val is_empty : 'a queue -> bool
    val enq : 'a -> 'a queue -> unit
    val deq : 'a queue -> 'a
    val to_list : 'a queue -> 'a list
    val length : 'a queue -> int
    val print : 'a queue -> ('a -> string) -> unit
    val from_list : 'a list -> 'a queue
    val contains : 'a -> 'a queue -> bool
    val truncate : 'a -> 'a queue -> unit
    val delete : 'a -> 'a queue -> unit
    val has_cycle : 'a queue -> bool
    val debug_name : string
  end
