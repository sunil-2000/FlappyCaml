open Graphics


let gravity = -1.5
let old_t = ref (Unix.gettimeofday())
let velocity = 4.0
let t_delta = 1.0
(* let min = bottom_of_screen *)

type t = {
  (* name : string;
     sprite : string; *)
  mutable position : (float * float);
  mutable velocity : float;
  (* orientation : float;
     mutable is_jump: bool; *)
}

let velocity_change player = 
  match player.velocity with
  | v -> player.velocity <- (v +. (gravity *. t_delta))

(* let calc_player_pos player = 
   let x = velocity_change player in
   let velocity = player.velocity in
   (**let t_delta = 0.01 (**Unix.gettimeofday() -. !old_t in*) in*)
   match player.position with 
   | (x, y) -> player.position <- 
      (x, y +. (velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0))) *)

let calc_player_pos player = 
  velocity_change player;
  (**let t_delta = 0.01 (**Unix.gettimeofday() -. !old_t in*) in*)
  match player.position with 
  | (x, y) -> player.position <- 
      (x, y +. (player.velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0))) 


let draw = 
  open_graph " 800x400";
  plot 5 5;
  set_window_title "Flappy Caml"; 

  set_color (rgb 255 0 0); ()

(* let circle = fill_circle 200 200 20; *)

let player_1 = {
  position = (400.,200.);
  velocity = velocity;
} 

let loop player = 
  while (snd player_1.position) > 1. do
    clear_graph ();
    let e = wait_next_event [Key_pressed] in
    if e.keypressed then
      player.velocity <- player.velocity +. velocity
    else 
      player.velocity <- player.velocity +. 0.0;
    calc_player_pos player_1;
    let x = int_of_float (fst player_1.position) in
    let y = int_of_float (snd player_1.position) in
    fill_circle x y 20;
  done 