open Gui
open Game 
open State

(* move some of these functions to state module *)
(* gui, player, state initial values *)
let gui_init = Gui.make_state 600 700 200 200 400 0 0 0 0
let player_init = Game.create (200., 200.) 5. 0 
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
let time_per_frame = 1. /. 30.
let dummy = ref 0

let fps_counter = 
  let d = (Unix.gettimeofday ()) -. !old_t_fps in 
  old_t_fps := Unix.gettimeofday ();
  last_update := d; 
  fps := 1. /. d

(* [state_go gui player delta_t] executes game properly if state of game
   = Go. *)
let state_go gui player delta_t frame = 
  let player' = 
    if (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = Game.update delta_t player' in 
  let y' = Game.get_y new_player |> int_of_float in
  let pipe_x' = Game.get_pipe new_player in
  let choose_pipe = Game.get_pipe_type new_player in
  let score' = Game.get_score new_player in
  let highscore = Game.get_highscore new_player in 
  let gui_update = Gui.update_fly y' score' frame pipe_x'  
      choose_pipe highscore gui in 

  Gui.make_gui gui_update;
  (new_player, gui_update) 

(* [state_run gui player delta_t] is a helper function for main that runs
   game properly when state = Run *)
let state_run gui player delta_t frame = 
  let player' = 
    if (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') && (Game.get_y player <= 100.) then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = Game.update_run delta_t player' in 
  let y' = Game.get_y new_player |> int_of_float in
  let pipe_x' = Game.get_pipe new_player in
  let score' = Game.get_score new_player in
  let gui_update = Gui.update_run y' score' frame pipe_x' gui in 

  Gui.make_gui gui_update;
  (new_player, gui_update) 

(** [state_torun gui player delta_t frame] that handles the game updates as it 
    transitions to the running game state. *)
let state_torun gui player delta_t frame = 
  failwith"TODO"

let next_frame = 
  fun () ->
  dummy := (!dummy) + 1;
  !dummy

(* [main gui player state] is responsible for executing the game properly when
   running *)
let rec main gui player state = 
  (* if now - lastupdatetime > time_between_updates && update_count < 1 (1 update per second) *)
  if Unix.gettimeofday () -. !old_t_fps > time_per_frame then 
    let curr_state = State.check state player in 

    let state' = curr_state |> State.get_state in 
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    old_t_fps := time_instant;
    frame_count := (!frame_count) + 1;
    if state' = Go then
      match state_go gui player delta_t (!frame_count) with 
      | (player, gui) -> 
        Unix.sleepf 0.001; 
        main gui player state
    else if state' = Run then 
      match state_run gui player delta_t (!frame_count) with 
      | (player, gui) -> main gui player state
    else if state' = GameOver then 
      begin 
        end_game gui player curr_state
      end 
    else main gui player state 
    (* if (Graphics.key_pressed ()) && (Graphics.read_key () = 'q') then 
       Graphics.close_graph ()
       else main gui player state  *)
  else 
    main gui player state 
(* else return unit *)
(* main gui player state *)

(* [start_game gui player state] runs game with start screen and then changes
   to go state when user executes a mouse click *)
and start_game gui player state = 
  Gui.draw_start gui;
  let curr_state = State.check state player in
  if State.get_state curr_state <> Go then 
    start_game gui player curr_state 
  else 
    main gui player curr_state 

(* [end_game gui player state] executes game when state = GameOver *)
and end_game gui player state = 
  Gui.draw_gameover gui;
  let state' = check state player in 
  if get_state state' <> Start then 
    end_game gui player state
  else 
    start_game gui_init (Game.create (200., 200.) 5. (Game.get_highscore player)) state_init

let () = 
  State.pick_interval player_init;
  start_game gui_init player_init state_init
