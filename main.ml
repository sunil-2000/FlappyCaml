open Gui
open Game 
open State

(* gui, player, and state initial values *)
let player_init = Game.create (200., 350.) 5. 0 "pipe" (Some (Random.int 3)) 600 0
let gui_init = Gui.make_state player_init
let state_init = State.make_state ()

(* [old_t] stores the time of the previous call to main, which 
   helps track time of game, which is used in the game module's gravity
   equation *)
let old_t = ref (Unix.gettimeofday ())

(* global variables used to track fps and regulate fps rate *)
let old_t_fps = ref (Unix.gettimeofday ())
let last_update = ref 0. 
let target_fps = 60.
let frame_count = ref 0
let time_per_frame = 1. /. target_fps 

(**************************DEBUG FUNCTIONS*************************************)
(******************************************************************************)
let fps = ref 0. 
let fps_counter = 
  let d = (Unix.gettimeofday ()) -. !old_t_fps in 
  old_t_fps := Unix.gettimeofday ();
  last_update := d; 
  fps := 1. /. d

let string_of_mouse_pos mouse = 
  match mouse with 
  | (x, y) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
(******************************************************************************)

(* [main gui player state] is responsible for executing the game properly when
   running *)
let rec main gui player state = 
  if Unix.gettimeofday () -. !old_t_fps >= time_per_frame then 
    let curr_state = State.check state player in 
    let state' = curr_state |> State.string_of_state in 
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    old_t_fps := time_instant;
    frame_count := (!frame_count + 1 ) mod 60; 
    Graphics.auto_synchronize true;
    match state' with 
    | "start" -> static_screen gui player curr_state 
    | "go" -> fly_bomb gui player curr_state delta_t (!frame_count)
    | "run" -> run gui player curr_state delta_t (!frame_count)
    | "bomb" -> fly_bomb gui player curr_state delta_t (!frame_count)
    | "death" -> death gui player curr_state delta_t 
    | "gameover" -> end_game gui player curr_state
    | "instructions" -> static_screen gui player curr_state
    | "sprites" -> static_screen gui player curr_state 
    | "sprite1" | "sprite2" | "sprite3" -> select_char gui player curr_state 
    | "torun" | "togo" | "tobomb" -> 
      transition gui player curr_state delta_t (!frame_count)
    | "dev" -> static_screen gui player curr_state
    | "quit" -> quit gui player curr_state 
    | "easter" -> static_screen gui player curr_state
    | _ -> failwith "state not implemented <- main"
  else 
    main gui player state 

(* [synchronize ()] synchronizes the frames to reduce stuttering *)
and synchronize () = 
  old_t_fps :=  Unix.gettimeofday ();
  Graphics.auto_synchronize true

(* [static_screen gui player state] renders the screen when the game is in a 
   "static" state. For example, this function is used for the start screen, 
   sprite screen, and instruction screen *)
and static_screen gui player state = 
  synchronize ();
  Gui.draw_update gui (State.string_of_state state);
  main gui player state

(* [end_game gui player state] executes game when state = GameOver *)
and end_game gui player state = 
  synchronize ();
  Gui.draw_update gui (State.string_of_state state);
  let state' = check state player in 
  if State.string_of_state state' <> "start" then 
    end_game gui player state
  else 
    let highscore = Game.get_highscore player in 
    let new_player = Game.create (200., 350.) 5. highscore "pipe" 
        (Some (Random.int 3)) 600 0 in 
    main gui_init new_player state_init

(* [run_fly_aux gui player state delta_t frame bool] is a helper function that 
   helps executes the game when state = Run or Go *)
and run_fly_aux gui player state delta_t frame bool =  
  let player' = 
    if bool then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = Game.update delta_t player' (string_of_state state) in 
  let gui_update = Gui.update_dynamic new_player frame gui in 

  Gui.draw_update gui_update (State.string_of_state state);
  main gui_update new_player state 

(* [fly gui player state delta_t frame] executes game properly when 
   state = Go or Bomb *)
and fly_bomb gui player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') in 
  run_fly_aux gui player state delta_t frame bool 

(* [state_run gui player delta_t] runs the game properly when state = Run *)
and run gui player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') 
             && (Game.get_player_y player <= 100) in 
  run_fly_aux gui player state delta_t frame bool 

(* [select_char gui player state] transitions game properly when user selects 
   a character *)
and select_char gui player state = 
  let array_no = 
    match State.string_of_state state with 
    | "sprite1" -> 1 
    | "sprite2" -> 2
    | "sprite3" -> 3
    | _ -> failwith "array not implemented <- [select_char]" in 
  let gui' = Gui.set_sprite gui array_no in 
  let state' = make_state () in 
  main gui' player state'

(* [transition_aux gui player state delta_t obs_name frame] is a helper function
   for transition that helps execute the game when state = ToGo or ToRun *)
and transition_aux gui player state delta_t obs_name frame  =
  let new_player = 
    Game.update delta_t player (string_of_state state) |> 
    Game.set_obs_type obs_name in 
  let gui' = Gui.update_dynamic new_player frame gui in 
  Gui.draw_update gui' (State.string_of_state state);
  main gui' new_player state

(* [transition_aux_bomb gui player state delta_t frame] is a helper function 
   for transition that helps execute the game when state = ToBomb *)
and transition_aux_bomb gui player state delta_t frame  =
  let new_player = Game.update delta_t player (string_of_state state) in
  let gui' = Gui.update_dynamic new_player frame gui in  
  Gui.draw_update gui' (State.string_of_state state);
  main gui' new_player state

(* [transition gui player state delta_t frame] executes the game when the 
   state is in a "transition" state, such as ToGo, ToRun, or ToBomb *)
and transition gui player state delta_t frame  = 
  match State.string_of_state state with 
  | "togo" -> transition_aux gui player state delta_t "pipe" frame
  | "torun" -> transition_aux gui player state delta_t "cactus" frame
  | "tobomb" -> transition_aux_bomb gui player state delta_t frame
  | _ -> failwith "transition"

(* [death gui player state delta_t] executes the game when state = Death *)
and death gui player state delta_t = 
  let player' = Game.update delta_t player (string_of_state state) in 
  Gui.draw_update gui (string_of_state state);
  main gui player' state

(* [quit gui player state] closes the game when player selects the "quit" button
   on the start screen *)
and quit gui player state = 
  Graphics.close_graph ()

let () = 
  Graphics.open_graph "600 700";
  Graphics.set_window_title "Flappy Caml"; 
  main gui_init player_init state_init
