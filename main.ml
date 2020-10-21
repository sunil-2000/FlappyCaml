let main () = 
  let gui = Gui.make_gui 600 400 in 
  Gui.test_img "assets/camel_test_sprite.ppm" gui 

let () = main ()