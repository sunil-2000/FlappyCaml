open Gui
open Game 
open State

(* gui, player, state initial values *)
let gui_init = Gui.make_state  200 200 400 0 0 0 
let player_init = Game.create (200., 200.) 5. 0 "pipe" (Some (Random.int 3)) 600 0
let state_init = State.make_state ()

(* [old_t] stores the time of the previous call to main, which 
   helps track time of game, which is used in the game module's gravity
   equation *)
let old_t = ref (Unix.gettimeofday ())

(* track fps *)
let old_t_fps = ref (Unix.gettimeofday ())
let fps = ref 0. 
let last_update = ref 0. 
let target_fps = 60.
let frame_count = ref 0
let time_per_frame = 1. /. 60.
let dummy = ref 0
let death_time = ref (Unix.gettimeofday())

(**************************DEBUG FUNCTIONS*************************************)
(******************************************************************************)
let fps_counter = 
  let d = (Unix.gettimeofday ()) -. !old_t_fps in 
  old_t_fps := Unix.gettimeofday ();
  last_update := d; 
  fps := 1. /. d

let next_frame = 
  fun () ->
  dummy := (!dummy) + 1;
  !dummy

let string_of_mouse_pos mouse = 
  match mouse with 
  | (x, y) -> "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
(******************************************************************************)

(* [main gui player state] is responsible for executing the game properly when
   running *)
let rec main (gui:Gui.t) player state = 
  (* if now - lastupdatetime > time_between_updates && update_count < 1 (1 update per second) *)
  if Unix.gettimeofday () -. !old_t_fps > time_per_frame then 
    (* should create helper for this part *)
    (**************************************)
    let curr_state = State.check state player in 
    let state' = curr_state |> State.string_of_state in 
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    old_t_fps := time_instant;
    frame_count := (!frame_count) + 1;
    (**************************************)
    (* factor into helper for pattern matches against state *)
    (* state is abstract so need to make method for getting string_of_state, and
       pattern match against that, haven't made mli yet which is why we can access
       type of state directly *)
    (**************************************)
    match state' with 
    | "start" -> start_game gui player curr_state 
    | "go" -> fly gui player curr_state delta_t (!frame_count)
    | "run" -> run gui player curr_state delta_t (!frame_count)
    | "bomb" -> bomb gui player curr_state delta_t (!frame_count)
    | "death" -> death gui player curr_state delta_t 
    | "gameover" -> end_game gui player curr_state
    | "instructions" -> instructions gui player curr_state
    | "sprites" -> sprites gui player curr_state 
    | "sprite1" | "sprite2" | "sprite3" -> select_char gui player curr_state
    | "torun" | "togo" | "tobomb" -> transition gui player curr_state delta_t
    | _ -> failwith "state not implemented <- main"
    (**************************************)

  (* if (Graphics.key_pressed ()) && (Graphics.read_key () = 'q') then 
     Graphics.close_graph ()
     else main gui player state  *)
  else 
    main gui player state 
(* else return unit *)
(* main gui player state *)

and synchronize () = 
  old_t_fps :=  Unix.gettimeofday ();
  Graphics.auto_synchronize true

and instructions gui player state = 
  synchronize ();
  Gui.draw_update gui (State.string_of_state state);
  main gui player state

and sprites gui player state = 
  synchronize ();
  Gui.draw_update gui (State.string_of_state state);
  main gui player state 

(* [start_game gui player state] runs game with start screen and then changes
   to go state when user executes a mouse click *)
and start_game gui player state = 
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
    let new_player = Game.create (200., 200.) 5. highscore "pipe" 
        (Some (Random.int 3)) 600 0 in 
    start_game gui_init new_player state_init

and run_fly_aux gui player state delta_t frame gamefn guifn bool =  
  let player' = 
    if bool then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = gamefn delta_t player' (string_of_state state) in 
  let gui_update = guifn new_player frame gui in 

  Gui.draw_update gui_update (State.string_of_state state);
  main gui_update new_player state 

(* executes game properly when state = Go *)
and fly (gui: Gui.t) player state delta_t frame = 

  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') in 
  run_fly_aux gui player state delta_t frame 
    Game.update Gui.update_fly bool 

(* [state_run gui player delta_t] is a helper function for main that runs
   game properly when state = Run *)
and run gui player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') 
             && (Game.get_y player <= 100.) in 
  run_fly_aux gui player state delta_t frame 
    Game.update Gui.update_run bool 

(* transitions state properly when user selects a character *)
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

and transition_aux gui player state delta_t game_updatefn obs_name =
  let new_player = game_updatefn delta_t player (string_of_state state) in 
  let player' = Game.set_obs_type new_player obs_name in 
  let gui' = Gui.update_torun player' (!frame_count) gui in 
  Gui.draw_update gui' (State.string_of_state state);
  main gui' player' state

and transition gui player state delta_t  = 
  match State.string_of_state state with 
  | "togo" -> transition_aux gui player state delta_t Game.update "pipe" 
  | "torun" -> transition_aux gui player state delta_t Game.update "cactus"
  | "tobomb" -> failwith "tobomb not implement in transition [main.ml]"
  | _ -> failwith "transition"

and death gui player state delta_t = 
  let player' = Game.update delta_t player (string_of_state state) in 
  Gui.draw_update gui (string_of_state state);
  main gui player' state

and bomb gui player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') in 
  run_fly_aux gui player state delta_t frame Game.update Gui.update_fly bool 

let () = 
  Graphics.open_graph "600 700";
  start_game gui_init player_init state_init
