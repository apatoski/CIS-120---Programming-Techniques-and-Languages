(** Paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES and PROGRAM STATE    *)
(******************************************)

(** The paint program uses the mutable record (called [state] below)
to store its state.  *)

(** A location in the paint_canvas widget *)
type point = position (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 2, 4, 5 and maybe 6. *)
type shape = 
  | Line of int * color * point * point
  | Point of color * point
  | Points of color * point list 
  | Ellipse of int* color * point * dimension

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

    - LineStartMode means the paint program is waiting for the user to make
    the first click to start a line.

    - LineEndMode means that the paint program is waiting for the user's
    second click. The point associated with this mode stores the location of
    the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 2, 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseStartMode
  | EllipseEndMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
  least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode  : mode;
  mutable width : int;
  (** The currently selected pen color. *)
  mutable color : color;
  mutable preview : shape option;

  (* TODO: You will need to add new state for Tasks 3, 5, and *)
  (* possibly 6 *) 
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  width = 1;
	preview = None;
  (* TODO: You will need to add new state for Tasks 3, 5, and maybe 6 *)
  
}



(** This function creates a graphics context with the appropriate
    pen color.
*)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (w:int) : gctx =
  let g = with_width (with_color g c) w in
  g

(*********************************)
(** PAINT CANVAS REPAINTING      *)
(*********************************)
(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)
    
(* TODO: You will need to modify this repaint function in Tasks 2, 3, 4,   *)
(* and possibly 6. For example, if the user is performing some operation   *)
(* that provides "preview" (see Task 2) the repaint function must also     *)
(* show the preview.                                                       *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line (w,c,p1, p2) -> draw_line (with_params g c w) p1 p2
      | Points (c,pl)   -> draw_points (with_params g c 1) pl
      | Ellipse (w,c,p,d) -> let (rx,ry) = d in draw_ellipse (with_params g c w) p rx ry
    end in
  Deque.iterate draw_shape paint.shapes;
  begin
    match paint.preview with
    | None -> ()
    | Some (x) -> draw_shape x
  end

(** Create the actual paint_canvas widget and its associated
notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint

(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur 
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let abs_val (x:int) =
  if x>0 then x else -x

let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
    (* This case occurs when the mouse has been clicked in the canvas, but *)
    (* before the button has been released. How we process the event       *)
    (* depends on the current mode of the paint canvas.                    *)
        begin match paint.mode with 
            | LineStartMode ->                                       
                paint.mode <- LineEndMode p
            | LineEndMode p1 ->()
            | PointMode ->  paint.preview <- Some (Points (paint.color,[p]))
            | EllipseStartMode ->                                                 
                paint.mode <- EllipseEndMode p
            | _ -> ()
            
          end
    | MouseDrag ->	
			    begin match paint.mode with 
            | LineStartMode -> ()
            | LineEndMode p1 ->
                paint.preview <- Some ( Line (paint.width,paint.color,p1,p))
            | PointMode ->
            let points =
            begin 
              match paint.preview with
              | None -> []
              | Some (Points (_, ps)) -> ps
            end 
            in
            paint.preview <- Some (Points (paint.color,p::points));
					  | EllipseStartMode -> ()
            | EllipseEndMode (x1,y1) ->
                let x,y = p in
                paint.preview <- Some ( Ellipse (paint.width,paint.color,(((x1+x)/2),((y1+y)/2)),(abs_val ((x-x1)/2),abs_val ((y-y1)/2))))
            
          end
    (* In this case, the mouse has been clicked, and it's being dragged    *)
    (* with the button down. Initially there is nothing to do, but you'll  *)
    (* need to update this part for Task 2, 3, 4 and maybe 6.                 *)
   
    | MouseUp ->
           begin match paint.mode with 
            | LineStartMode ->
                (* The paint_canvas was waiting for the first click of a line,   *)
                (* so change it to LineEndMode, recording the starting point of  *)
                (* the line.                                                     *)
                ()
            | LineEndMode p1 ->
                (* The paint_canvas was waiting for the second click of a line,  *)
                (* so create the line and add it to the deque of shapes. Go back *)
                (* to waiting for the first click. *)
                (Deque.insert_tail (Line (paint.width,paint.color, p1, p)) paint.shapes;
                paint.mode <- LineStartMode;
                paint.preview <- None)
            | PointMode -> begin match  paint.preview with
						              |Some (x)-> 
							(Deque.insert_tail x) paint.shapes; paint.preview <-None
							|None->()
							end;
              paint.preview <- None
            | EllipseStartMode ->
                                                                   
                ()
            | EllipseEndMode p1 ->
                (* The paint_canvas was waiting for the second click of a line,  *)
                (* so create the line and add it to the deque of shapes. Go back *)
                (* to waiting for the first click. *)
                begin match  paint.preview with
                          |Some (x)-> 
              (Deque.insert_tail x) paint.shapes; paint.preview <-None
              |None->()
            end;
                paint.preview <- None; paint.mode <- EllipseStartMode
          end
    (* In this case there was a mouse button release event. TODO: Tasks 2, *)
    (* 3, 4, and possibly 6 need to do something different here.           *)
	 
    | _ -> ()
    
    (* This catches the MouseMove event (where the user moved the mouse over *) 
    (* the canvas without pushing any buttons) and the KeyPress event (where *)
    (* the user typed a key when the mouse was over the canvas).             *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action

(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(**
This part of the program creates the other widgets for the
paint program -- the buttons, color selectors, etc., and
lays them out in the top - level window.

*)
(* TODO: Tasks 1, 2, 4, 5, and 6 involving adding new buttons or changing  *)
(* the layout of the Paint GUI. Initially the layout is very ugly because  *)
(* we use only the hpair widget demonstrated in Lecture. Task 1 is to make *)
(* improvements to make the layout more appealing. You may choose to       *)
(* arrange the buttons and other GUI elements of the paint program however *)
(* you like (so long as it is easily apparent how to use the interface ).  *)
(* The sample screen shot of our solution provides one possible design.    *)
(* Also, feel free to improve the visual components of the GUI, for        *)
(* example, our solution puts borders around the buttons and uses a custom *)
(* "color button" that changes its appearance based on whether or not the  *)
(* color is currently selected.                                            *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"
let (w_point, lc_point, nc_point) = button "Point"
let (w_line, lc_line, nc_line) = button "Line"
let (w_el, lc_el, nc_el) = button "Ellipse"
(**
This function runs when the Undo button is clicked.
It simply removes the last shape from the shapes deque.
*)
(* TODO: You need to modify this in Task 3 and 4. *)
let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes); paint.preview <-None; if(paint.mode!=LineStartMode) then paint.mode<-LineStartMode

let point () : unit =
  paint.mode <- PointMode

let line () : unit =
  paint.mode <- LineStartMode

  let ellipse () : unit =
  paint.mode <- EllipseStartMode

;; nc_undo.add_event_listener (mouseclick_listener undo)
;; nc_line.add_event_listener (mouseclick_listener line)
;; nc_point.add_event_listener (mouseclick_listener point)
;; nc_el.add_event_listener (mouseclick_listener ellipse)
(** The Quit button, with associated functionality. *)
let w_quit, lc_quit, nc_quit = button "Quit"

;; nc_quit.add_event_listener (mouseclick_listener (fun () -> exit 0))

(** A spacer widget *)
let spacer : widget = space (10,10)



(** The mode toolbar, initially containing just the Undo and Quit buttons. *)
(*  TODO: you will need to add more buttons to the toolbar in *)
(*  Tasks 2,5, and 6. *)

let rw_line = checkbox false "Line" 
let rw_line1,rw_line2 = fst rw_line,snd rw_line
;; rw_line2.add_change_listener (fun x -> if x then line() )

let rw_point = checkbox false "Point"
let rw_point1,rw_point2 = fst rw_point,snd rw_point
;; rw_point2.add_change_listener  (fun x -> if x then point())

let rw_el= checkbox false "Ellipse" 
let rw_el1,rw_el2 = fst rw_el,snd rw_el
;; rw_el2.add_change_listener  (fun x -> if x then ellipse())

let cb = checkbox false "Thick?" 
let thick,cnt = fst cb,snd cb
;;cnt.add_change_listener (fun b -> if b then paint.width <- 10 else paint.width <-1)
let test =fst ( radio_button [checkbox false "Thick?" ;checkbox false "mook?" ;checkbox true "Th" ;] 2 "lol" )

let mode_toolbar = hlist [w_el;spacer;w_line;spacer;w_point;spacer;w_undo;spacer;w_quit;spacer;thick]


let mt = radio_button [rw_point;rw_line;rw_el] 1 ""


(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color 
   and some buttons for changing it. Both the indicator and the buttons 
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given 
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
    : widget * notifier_controller =
  let repaint_square (gc:gctx) =
	 let c = get_color () in
    fill_rect (with_color gc c) (0, width-1) (width-1, width-1) in   
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected 
   color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created 
   with. They are also installed with a mouseclick listener
   that changes the selected color of the paint app to their color. *)  
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
    paint.color <- c ));
  w
(** The color selection toolbar. Contains the color indicator and 
    buttons for several different colors. *)
let color_toolbar : widget =
   hpair (hpair color_indicator spacer)
	
 ( vlist [hlist [color_button black;spacer;color_button white;spacer;color_button red;
spacer;color_button green];space(4,4);
  hlist [color_button blue;spacer;color_button yellow;
	 spacer;color_button cyan;spacer;color_button magenta]])

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets.
*)
(* TODO: Task 1 (and others) modify the layout to add new buttons and make *)
(* the layout more aesthetically appealing.                                *)
let paint_widget =
  vlist [hlist[spacer;w_quit;spacer;w_undo];space(2,2); paint_canvas;spacer;
	hlist[spacer;fst mt;spacer;thick;spacer;];
	spacer;hlist [spacer;color_toolbar]]
(**************************************)
(** Start the application             *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
