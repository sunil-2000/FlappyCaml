open Gui
open Game 
open State

(* move some of these functions to state module *)
(* gui, player, state initial values *)
let gui_init = Gui.make_state 600 700 200 200 400 0 0 0 0
let player_init = Game.create (200., 200.) 5. 0 "pipe" (Some (Random.int 3)) 600 
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

(** [state_torun gui player delta_t frame] that handles the game updates as it 
    transitions to the running game state. *)

(* [main gui player state] is responsible for executing the game properly when
   running *)
let rec main (gui:Gui.t) player state = 
  (* if now - lastupdatetime > time_between_updates && update_count < 1 (1 update per second) *)
  if Unix.gettimeofday () -. !old_t_fps > time_per_frame then 
    (* should create helper for this part *)
    (**************************************)
    let curr_state = State.check state player in 
    let state' = curr_state |> State.get_state in 
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    old_t_fps := time_instant;
    frame_count := (!frame_count) + 1;
    print_string (State.string_of_state curr_state); 
    (**************************************)
    (* factor into helper for pattern matches against state *)
    (* state is abstract so need to make method for getting string_of_state, and
       pattern match against that, haven't made mli yet which is why we can access
       type of state directly *)
    (**************************************)
    match state' with 
    | Start -> start_game gui player curr_state 
    | Go -> fly gui player curr_state delta_t (!frame_count)
    | Run -> run gui player curr_state delta_t (!frame_count)
    | Death -> death gui player curr_state delta_t 
    | GameOver -> end_game gui player curr_state
    | Instructions -> instructions gui player curr_state
    | Sprites -> sprites gui player curr_state 
    | Sprite1 | Sprite2 | Sprite3 -> select_char gui player curr_state
    | ToRun -> transition gui player curr_state delta_t
    | ToGo -> transition gui player curr_state delta_t 
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
  Gui.draw_instructions gui;
  main gui player state

and sprites gui player state = 
  synchronize ();
  Gui.draw_sprites gui;
  main gui player state 

(* [start_game gui player state] runs game with start screen and then changes
   to go state when user executes a mouse click *)
and start_game gui player state = 
  synchronize ();
  Gui.draw_start gui;
  main gui player state 

(* [end_game gui player state] executes game when state = GameOver *)
and end_game gui player state = 
  synchronize ();
  Gui.draw_gameover gui;
  let state' = check state player in 
  if get_state state' <> Start then 
    end_game gui player state
  else 
    let highscore = Game.get_highscore player in 
    let new_player = Game.create (200., 200.) 5. highscore "pipe" 
        (Some (Random.int 3)) 600 in 
    start_game gui_init new_player state_init

and run_fly_aux (gui:Gui.t) player state delta_t frame gamefn guifn bool gmake =  
  let player' = 
    if bool then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = gamefn delta_t player' in 
  let y' = Game.get_y new_player |> int_of_float in
  let pipe_x' = Game.get_obs_x new_player in
  let choose_pipe = Game.get_pipe_type new_player in
  let score' = Game.get_score new_player in
  let highscore = Game.get_highscore new_player in 
  let gui_update = guifn y' score' frame pipe_x' choose_pipe highscore gui in 

  gmake gui_update;
  main gui_update new_player state 

(* executes game properly when state = Go *)
and fly (gui: Gui.t) player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') in 
  run_fly_aux gui player state delta_t frame Game.update Gui.update_fly bool Gui.make_gui 

(* [state_run gui player delta_t] is a helper function for main that runs
   game properly when state = Run *)
and run gui player state delta_t frame = 
  let bool = (Graphics.key_pressed ()) && (Graphics.read_key () = '\032') 
             && (Game.get_y player <= 100.) in 
  run_fly_aux gui player state delta_t frame Game.update_run Gui.update_run bool Gui.draw_run 

(* transitions state properly when user selects a character *)
and select_char gui player state = 
  let array_no = 
    match State.get_state state with 
    | Sprite1 -> 1 
    | Sprite2 -> 2
    | Sprite3 -> 3
    | _ -> failwith "array not implemented <- [select_char]" in 
  let gui' = Gui.set_sprite gui array_no in 
  let state' = make_state () in 
  main gui' player state' 

(* get rid of redundant code in fly / run *)

and transition_aux gui player state delta_t game_updatefn obs_name =
  let new_player = game_updatefn delta_t player in 
  let player' = Game.set_obs_type new_player obs_name in 
  let y' = Game.get_y player |> int_of_float in
  let score' = Game.get_score player in
  let highscore = Game.get_highscore player in 
  let gui' = Gui.update_torun y' score' (!frame_count) highscore gui in 
  Gui.make_gui gui';
  main gui' player' state

and transition gui player state delta_t  = 
  match state.state with 
  | ToGo -> transition_aux gui player state delta_t Game.update_togo "pipe" 
  | ToRun -> transition_aux gui player state delta_t Game.update_torun "cactus"
  | _ -> failwith "transition"

and death gui player state delta_t = 
  let set_dimg = Gui.set_sprite gui 4 in 
  let player' = Game.gravity_fly delta_t player in 
  let y' = Game.get_y player |> int_of_float in 
  let gui' = Gui.update_death set_dimg y' in 
  Gui.draw_death gui';
  main gui' player' state



let () = 
  Graphics.open_graph "600 700";
  State.pick_interval player_init;
  start_game gui_init player_init state_init
