open Gui

let main () = 
  let test = make_state 600 700 100 200 in
  make_gui test;
  ()

let () = main ()