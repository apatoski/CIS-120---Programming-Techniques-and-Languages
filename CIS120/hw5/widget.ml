(** A library of widgets for building GUIs. *)

(************)
(** Widgets *)
(************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions

*)

;;open List

type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(** {1 Layout Widgets } *)
(************************)

(** The simplest widget just occupies space *)
let space (p: Gctx.dimension) : widget =
  { repaint = (fun _ -> ());
    handle = (fun _ _ -> ());
    size = (fun _ -> p);
  }
let spacer : widget = space (10,10)
(** Adds a one-pixel border to an existing widget. *)
let border (w: widget) : widget =
  { repaint = (fun (g: Gctx.gctx) ->
        let (width, height) = w.size () in
        let x = width + 3 in    (* not + 4 because we start at 0 *)
        let y = height + 3 in
        Gctx.draw_line g (0,0) (x,0);
        Gctx.draw_line g (0,0) (0, y);
        Gctx.draw_line g (x,0) (x, y);
        Gctx.draw_line g (0, y) (x, y);
        let g = Gctx.translate g (2,2) in
        w.repaint g);

    handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
            w.handle (Gctx.translate g (2,2)) e);

    size = (fun () ->
            let (width, height) = w.size () in
            width + 4, height + 4);
  }

(* Determines whether a given event is within a region of a widget whose*)
(* upper-left hand corner is (0,0) with width w and height h.           *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally.  They
    are aligned at their top edge. *)
let hpair (w1:widget) (w2:widget) : widget = {
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (fst (w1.size ()),0) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (fst (w1.size ()), 0)) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (x1 + x2, max y1 y2))
}

let vpair (w1: widget) (w2: widget) : widget =
{
  repaint = (fun  (g:Gctx.gctx) -> w1.repaint g;
    let g = Gctx.translate g (0,snd(w1.size ())) in
      w2.repaint g);
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
    if event_within g e (w1.size ())
    then w1.handle g e
    else let g = (Gctx.translate g (0, snd (w1.size ()))) in
         if event_within g e (w2.size ()) then w2.handle g e else ());
  size = (fun () -> let (x1,y1) = w1.size () in
          let (x2,y2) = w2.size () in (max x1 x2, y1 + y2))
}


(* Note: the OCaml List module has a function fold_right (List.fold_right).
   Note the order of the arguments (which is different
   from previous homeworks).
   Also, you will see that there is a fold_left function, you
   may want to think about what this does, and how it's different
   from the fold you're used to.
*)
let list_layout
    (pair: widget -> widget -> widget)
    (ws: widget list) : widget =
  fold_right pair ws (space (0,0))

let vlist (ws: widget list) : widget = list_layout (fun x y -> vpair x y) ws
let hlist (ws: widget list) : widget = list_layout (fun x y -> hpair x y) ws


(*****************************)
(** {1    Label Widgets    } *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The mechanism
   for this is called the "value_controller", which is generic to accomodate
   values of different types. (Don't worry about add_change_listener for now.)

  Because both the widget and the controller share the same, mutable value,
  the constructor must create both together. For label widgets, the value
  we're dealing with is of type string. *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = { get_label : unit -> string;
                          set_label : string -> unit }

(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  let r = { contents = s } in
  { repaint = (fun (g: Gctx.gctx) ->
            Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }

(*****************************************)
(** {1   Event Listeners and Notifiers } *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()


(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
      begin match Gctx.event_type e with
        | Gctx.KeyPress key -> action key
        | _ -> ()
      end

(** A notifier_controller is associated with a notifier widget.
    It allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
    extra screen space -- it extends an existing widget with the
    ability to react to events. It maintains a list of of "listeners"
    that eavesdrop on the events propagated through the notifier
    widget.

    When an event comes in to the notifier, it is passed to each
    event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = { contents = [] } in
  { repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
          List.iter (fun h -> h g e) listeners.contents;
          w.handle g e);
    size = w.size
  },
  { add_event_listener =
      fun (newl: event_listener) ->
          listeners.contents <- newl :: listeners.contents
  }

(*****************************************)
(** {1   Button                        } *)
(*****************************************)

(** A button has a string, which can be controlled by the
    corresponding value_controller, and an event listener, which can be
    controlled by the notifier_controller to add listeners (e.g. a
    mouseclick_listener) that will perform an action when the button is
    pressed. *)
let button (s: string)
    : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)

(*****************************************)
(** {1   Canvas                        } *)
(*****************************************)

(** A Canvas is a bordered widget with a notifier. New event listeners
    can be added by the notifier_controller.
    The repaint method of a canvas is a parameter of the widget constructor. *)
let canvas (dim: Gctx.dimension) (paint : Gctx.gctx -> unit)
    : widget * notifier_controller =
  let w =
    { repaint = paint;
      handle = (fun _ _ -> ());
      size = (fun _ -> dim) }
  in
  notifier (border w)

(*****************************************)
(** {1   Checkbox                      } *)
(*****************************************)
(* TODO: Task 5 requires you to develop a checkbox widget *)

(** A checkbox is a controller for a value associated with a widget.

    This controller can read and write the value. It also allows
    change listeners to be registered by the application. These listeners are
    run whenever this value is set. *)


type 'a change_listener = 'a -> unit

type 'a value_controller = {
  add_change_listener : 'a change_listener -> unit;
  get_value           : unit -> 'a;
  set_value           : 'a -> unit
}


let checkbox (init: bool) (s: string)
    : widget * bool value_controller =
  (*button+label combo with some local state funcionality*)
  let (w_cb,l_cb,n_cb) = if init then button "X" else button " " in
	let lbl = fst (label s) in
	let checked = {contents= if init then true else false;} in
  let change_listeners :  bool change_listener list ref = {contents=[]} in
	let bvc = 
		{ 
    add_change_listener = (
      fun (newl: bool change_listener) ->
      change_listeners.contents <- newl :: change_listeners.contents );
			
    get_value = ( fun () -> checked.contents);
    set_value = (fun (b: bool) -> checked.contents <- b; 
		             if checked.contents then l_cb.set_label "X" else l_cb.set_label " ";
								List.iter (fun x -> x checked.contents) change_listeners.contents)
  }
 in
 (*built in click listener as we always want this behavior with checkboxes*)
	n_cb.add_event_listener 
	(mouseclick_listener (fun () ->
   if checked.contents then bvc.set_value false else bvc.set_value true));  
	(*formatting*)
		border (hlist [border w_cb;space(2,2);lbl]),bvc
 


(*****************************************)
(** {1   Additional widgets            } *)
(*****************************************)
type  radio_change_listener = int change_listener 

type  radio_controller = {
  add_radio_change_listener :  radio_change_listener -> unit;
  get_selection           : unit -> int;
  set_selection           : int -> unit
}

let rec ith_elem (i:int) (l:'a list) =
  begin
    match l with
    | []   -> failwith "out of bounds"
    | h::t -> if (i=1) then h else ith_elem (i-1) t
  end
	
let rec transform (f: 'a -> 'b) (l: 'a list) : 'b list =
  begin match l with
  | [] -> []
  | x :: xs -> f x :: transform f xs
  end

(**RADIO BUTTON GROUPS**)
(**this interpretation of radio button enables a user to create a radio_button_group from :**)
(** a list of checkboxes, an index of initial checked box and label**)

let radio_button_group (cbl: (widget * bool value_controller) list) (init: int)  (s:string) : widget *  radio_controller =
  (*radio button has a label*)
	let lbl = fst (label s) in
	(*we are going to need separate control over widget and controler parts*)
  let cbwidgets,cbcontrols  = List.split cbl in
	(*now we are also going to introduce list of listeners: checkbox change -> radio_button change*)
	(*let select (i:int) = List.iter(fun (x:bool value_controller)-> x.set_value false) cbcontrols;
                      
                       (ith_elem i cbcontrols).set_value true in*)
  
 
	(*index of currently selected checkbox*)
  let selection = {contents = 1} in
	(*listeners*)
  let radio_change_listeners :   radio_change_listener list ref = {contents=[]} in
	(*control part*)
  let rc = 
    { 
      
		add_radio_change_listener = 
				(
          fun (newl:  radio_change_listener) ->
          radio_change_listeners.contents <- newl :: radio_change_listeners.contents 
				);
      
    get_selection = 
			  ( fun () -> selection.contents);
		(*ensures radio button invariants, links state of radio button to the states of individual checkboxes*)
    set_selection = fun (b: int) -> 
			  (
          List.iter(fun (x:bool value_controller)-> 
			    if( (ith_elem b cbcontrols)!= x ) then x.set_value false) cbcontrols;
		 		  selection.contents <- b; 
          List.iter (fun x -> x selection.contents) radio_change_listeners.contents;
											 
			  );
	
  } 
	in
	let allfalse (): bool = ( List.filter (fun x -> x.get_value()) cbcontrols =[] ) in
	(*enables radio button controler to "spy" on checkboxes*)
	let rec forr (i:int) (xx:'a list) =
  begin
    match xx with
    | []   -> ()
    | h::t -> h.add_change_listener (fun x -> if x then rc.set_selection i else if x=false then (if allfalse() then h.set_value true )); forr (i+1) t
  end
  in
	forr 1 cbcontrols;
	(*init*)
	rc.set_selection init;(ith_elem init cbcontrols).set_value true;
	(*formating*)
  (hlist [lbl;hlist cbwidgets],rc)

 (*alias for easier typing *)
	let radio_button=radio_button_group




(* TODO: In Task 6 you may choose to add a radio_button widget, color slider *)
(* or text_box widget.                                                       *)

