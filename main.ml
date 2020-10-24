open Gui

let jump' player =
  let key = Graphics.wait_next_event [Key_pressed] in
  if key.keypressed then 
    Game.jump player
  else 
    player

(* put this function in gui.ml later, as helper for [draw_player] *)
let gravity_draw player = 
  match Game.get_position player with
  |(x,y) ->  
    Graphics.clear_graph ();
    let a = int_of_float x in
    let b = int_of_float y in
    let test = make_state 600 700 a b in
    make_gui test;
    Unix.sleepf 0.01;
    print_string "done";
    Game.gravity player


(* if key pressed, call jump and run player through gravity function again*)
let rec move_player player = 
  match Game.get_position player with 
  |(x,y) -> 
    print_string "if";
    if y < 0. then 
      Graphics.clear_graph ()
    else 
    if (Graphics.key_pressed ()) && (Graphics.read_key () = 'v') then 
      Game.jump player 
      |> gravity_draw  
      |> move_player 
    else 
      move_player (gravity_draw player)


let main () = 
  (*let gui = Gui.make_gui 600 400 *)
  Graphics.open_graph " 600x700";
  let player = Game.create_t (200., 200.) 5. in 
  Gui.draw_player player

let () = main ()