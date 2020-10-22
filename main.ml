<<<<<<< HEAD
open Gui

let main () = 
  let test = make_state 600 700 100 200 in
  make_gui test;
  ()
=======
let jump' player =
  let key = Graphics.wait_next_event [Key_pressed] in
  if key.keypressed then 
    Game.jump player
  else 
    player

(* if key pressed, call jump and run player through gravity function again*)
let rec move_player player = 
  match Game.get_position player with 
  |(x,y) -> 
    print_string "if";
    if y < 0. then 
      Graphics.close_graph () 
    else 
      let player' = Game.gravity player in 
      match Game.get_position player with
      |(x,y) ->  
        Graphics.clear_graph ();
        Graphics.fill_circle (int_of_float x ) (int_of_float y) 10;
        Unix.sleepf 0.01;
        print_string "done";
        move_player player'

let move player = 
  move_player player


let main () = 
  (*let gui = Gui.make_gui 600 400 *)
  Graphics.open_graph " 600x400";
  let player = Game.create_t (200., 200.) 20. in 
  move_player player


>>>>>>> 4a95d18d727ec347bfa225ea572e43035ceb3ab3

let () = main ()