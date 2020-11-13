open Gui
open Game 
open State

(* move some of these functions to state module *)

(* old_t helps track time of game, which is used in the game module's gravity
   equation *)
let old_t = ref (Unix.gettimeofday ())

(* [check_mouse_click] returns true if a mouse click has occured *)
let rec check_mouse_click () = 
  (Graphics.button_down ()) && (check_mouse_click () = false )

(* [state_to_go] transitions the state from start to go *)
let state_to_go () = 
  match Graphics.mouse_pos () with 
  | (x, y) -> 
    (* dimensions of button <- should make field in gui for button *)
    if x > 0 && x < 600 && y > 0 && y < 700 && check_mouse_click () then 
      true 
    else 
      false

(* [check state player] returns the correct state of the game at given instance *)
let check state player = 
  if (Game.get_y player < 100. && State.get_state state = Go) || Game.get_collision player then 
    {state with state = GameOver}
  else if Game.get_score player = -10 then 
    State.set_run state 
  else if State.get_state state = Start then 
    if state_to_go () then 
      {state with state = Go}
    else 
      state 
  else 
    state

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

let state_start gui =
  Gui.draw_start gui

let rec main gui player state = 
  let curr_state = check state player in 
  let state' = curr_state |> State.get_state in 
  let time_instant = Unix.gettimeofday () in
  let delta_t = time_instant -. !old_t in
  old_t := time_instant;
  if state' = Go then
    match state_go gui player delta_t with 
    | (player, gui) -> main gui player state
  else if state' = Run then 
    match state_run gui player delta_t with 
    | (player, gui) -> main gui player state
  else 
    Graphics.close_graph ()

let rec start_game gui player state = 
  let curr_state = check state player in
  if State.get_state curr_state <> Go then start_game gui player curr_state 
  else 
    main gui player curr_state 

let () = 
  let gui_init = Gui.make_state 600 700 200 200 400 0 0 0 in 
  let player = Game.create (200., 200.) 5. in 
  let state_init = State.make_state () in 
  state_start gui_init; 
  start_game gui_init player state_init
