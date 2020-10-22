open Gui

let main () = 
  let test = make_state 600 700 100 200 in
  make_gui test;
  ()
(** Gui.test_img "assets/camel_test_sprite.ppm" gui *)

let () = main ()