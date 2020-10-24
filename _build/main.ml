(** main.ml responsible for running entire game *)
let main () = 

  Graphics.open_graph " 600x400";
  let player = Game.create_t (200., 200.) 5. in 
  Gui.draw_player player

let () = main ()