open Printf

type direction = Forward | Backward ;;

type pres_state =
    Display
  | Transition of direction * float
;;

let trans_time = 0.3 ;;
let cur_state = [| Display |] ;;

let reshape ~w ~h =
  GlDraw.viewport ~x:0 ~y:0 ~w:w ~h:h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(-1., 1.) ~y:(-1., 1.) ~z:(-1., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ();
  Glut.postRedisplay ();
;;

let new_state state =
  cur_state.(0) <- state;
  ()
;;

let keyboard ~key ~x ~y =
  Glut.postRedisplay ();
   match key with
     27 (* ESC *)       -> exit 0
   | 8  (* Backspace *) -> begin print_endline "BKWD!"; new_state (Transition(Backward, Unix.gettimeofday ())) end
   | _ -> begin print_endline "FWD!"; new_state (Transition(Forward, Unix.gettimeofday ())) end ;
  ()
;;

let page () =
  GlDraw.color (1., 1., 1.);            (* white bg *)
  GlDraw.rect (-0.99, 0.99) (0.99, -0.99);
  GlDraw.color (1., 0., 0.);            (* red part*)
  GlDraw.rect (-0.5, 0.5) (0.5, 0.);
  GlDraw.color (0., 0., 1.);            (* blue part *)
  GlDraw.rect (-0.5, 0.) (0.5, -0.5);
  
  Display
;;

let render_transition dir start =
  let elapsed = Unix.gettimeofday () -. start in
  if elapsed > trans_time then page ()
  else 
  let completed = match dir with
                  Forward -> elapsed /. trans_time
                  | Backward -> 1. -. (elapsed /. trans_time)
  in begin

  Glut.postRedisplay ();

  (* New page *)
  GlMat.push ();
  GlMat.translate3 ((completed -. 1.), 0., 0.);
  GlMat.translate3 (0., 1., 0.);
  GlMat.scale3 (1., completed, 1.);
  GlMat.translate3 (0., -. 1., 0.);
  ignore ( page () );
  GlMat.pop ();

  (* Old page *)
  GlMat.push ();
  GlMat.translate3 (0., -. 1., 0.);
  GlMat.scale3 (1. -. completed, 1. -. completed, 1.);
  GlMat.translate3 (0., 1., 0.);
  ignore ( page () );
  GlMat.pop ();

  Transition(dir, start)
  end
;;

let do_state state = 
  match state with
  Display -> page ();
  | Transition (dir, start) -> render_transition dir start;
;;

let render () = 
  GlMat.mode `modelview;
  GlMat.load_identity ();
  GlClear.color (0., 0., 0.1);
  GlClear.clear [ `color ];

  cur_state.(0) <- do_state cur_state.(0);

  Glut.swapBuffers ();
;;

let _ =
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~alpha:true  ~double_buffer:true ();
  Glut.initWindowSize ~w:400 ~h:300;
  ignore (Glut.createWindow ~title:"OpenGL Demo");
  Glut.keyboardFunc keyboard;
  Glut.reshapeFunc reshape;
  Glut.displayFunc render;
  
  (* Glut.idleFunc ~cb:(Some Glut.postRedisplay); *)
  Glut.mainLoop ()
;;

    (* EOF *)
