open Gui
open Game 
open State

(* move some of these functions to state module *)

(* [old_t] stores the time of the previous call to main, which 
   helps track time of game, which is used in the game module's gravity
   equation *)
let old_t = ref (Unix.gettimeofday ())

(* [state_go gui player delta_t] executes game properly if state of game
   = Go. *)
let state_go gui player delta_t = 
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
  let gui_update = Gui.update_fly y' score' (Gui.update_index gui) pipe_x' 
      choose_pipe gui in 

  Gui.make_gui gui_update;
  (new_player, gui_update) 

(* [state_run gui player delta_t] is a helper function for main that runs
   game properly when state = Run *)
let state_run gui player delta_t = 
  let player' = 
    if (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = Game.update_run delta_t player' in 
  let y' = Game.get_y new_player |> int_of_float in
  let score' = Game.get_score new_player in
  let gui_update = Gui.update_run y' score' (Gui.update_index gui) gui in 

  Gui.make_gui gui_update;
  (new_player, gui_update) 

(* [state_start gui] executes code properly when state = Start *)
let state_start gui =
  Gui.draw_start gui

(* [state_over gui] responsible for executing code when state = Gameover *)
let state_over gui = 
  Gui.draw_gameover gui

(* [main gui player state] is responsible for executing the game properly when
   running *)
let rec main gui player state = 

  let curr_state = State.check state player in 
  let state' = curr_state |> State.get_state in 
  if state' = Go then
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    match state_go gui player delta_t with 
    | (player, gui) -> main gui player state
  else if state' = Run then 
    let time_instant = Unix.gettimeofday () in
    let delta_t = time_instant -. !old_t in
    old_t := time_instant;
    match state_run gui player delta_t with 
    | (player, gui) -> main gui player state
  else
    Gui.draw_gameover gui;
  if (Graphics.key_pressed ()) && (Graphics.read_key () = 'q') then 
    Graphics.close_graph ()
  else main gui player state 


(* [start_game gui player state] runs game with start screen and then changes
   to go state when user executes a mouse click *)
let rec start_game gui player state = 
  let curr_state = State.check state player in
  if State.get_state curr_state <> Go then 
    start_game gui player curr_state 
  else 
    main gui player curr_state 

let () = 
  let gui_init = Gui.make_state 600 700 200 200 400 0 0 0 in 
  let player = Game.create (200., 200.) 5. in 
  let state_init = State.make_state () in 
  state_start gui_init; 
  start_game gui_init player state_init
