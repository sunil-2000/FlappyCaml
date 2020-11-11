open Gui
open Game 
open State

let old_t = ref (Unix.gettimeofday ())

(* new_gui = Gui.make_state 600 700 200 Game.gravity t_delta player pipe_change
   player  *)

let rec main gui player = 
  let time_instant = Unix.gettimeofday () in
  let delta_t = time_instant -. !old_t in
  old_t := time_instant;
  (* temporary conditional to close graph, eventually need to do something with 
     state module to transition to different state *)
  if Game.get_y player < 100. then Graphics.close_graph () else ();
  (* in if statement so that game ends *)
  (* update gui and update player *)
  let player' = if (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') then 
      Game.set_can_jump player true
    else 
      player in 

  let new_player = Game.update delta_t player' in 
  let y' = Game.get_y new_player |> int_of_float in
  let pipe_x' = Game.get_pipe new_player in
  let choose_pipe = Game.get_pipe_type new_player in

  let gui_update = Gui.make_state 600 700 200 y' pipe_x' choose_pipe in 
  Gui.make_gui gui_update; 
  Unix.sleepf(0.01);
  main gui_update new_player 

let () = 
  let gui_init = Gui.make_state 600 700 200 200 400 0 in 
  let player = Game.create (200., 200.) 5.  in 
  main gui_init player 
