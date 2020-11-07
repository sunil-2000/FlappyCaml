open Gui
open Game 
open State

let old_t = ref (Unix.gettimeofday ())
let player = Game.create_t (200., 200.) 5.

let rec main player = 
  let time_instant = Unix.gettimeofday () in
  let delta_t = time_instant -. !old_t in
  print_float delta_t;
  old_t := time_instant;
  (*let gui = Gui.make_gui 600 400 *)

  if State.game_over player = false then 
    let p' = Gui.draw_player delta_t player in 
    main p'
  else 
    Graphics.close_graph


<<<<<<< HEAD
let player = Game.create_t (200., 200.) 5. 
=======
let main () = 
  (*let gui = Gui.make_gui 600 400 *)
  let player = Game.create_t (200., 200.) 5. in 
  Gui.draw_player player
>>>>>>> f12c63cc4ee1b0faf46c9d9445347642f32adf5e

let () = 
  Graphics.open_graph " 600x700"; 
  main player ()