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


let player = Game.create_t (200., 200.) 5. 

let () = 
  Graphics.open_graph " 600x700"; 
  main player ()