<<<<<<< HEAD

let gravity = -100.
(* let old_t = ref (Unix.gettimeofday()) *)
let max_down = -180.
let jump_v = 100.
=======
let gravity = -1.
let old_t = ref (Unix.gettimeofday())
let max_down = -1.5
let jump_v = 5.
>>>>>>> f12c63cc4ee1b0faf46c9d9445347642f32adf5e
(* let t_delta = 1.0 *)
(* let min = bottom_of_screen *)

type t = {
  (* name : string;
     sprite : string; *)
  position : (float * float);
  velocity : float;
  (* orientation : float;
     mutable is_jump: bool; *)
  pipe_x : int;
  game_over : bool;
  is_jump : bool 
}

let create_t pos v = {
  position = pos;
  velocity = v;
  pipe_x = 600;
  game_over = false;
  is_jump = false
}

let is_gameover t = 
  t.game_over

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let get_pipe player = 
  player.pipe_x

let velocity_change t_delta player =  
  max (player.velocity +. (gravity *. t_delta)) max_down

let pipe_change player = {
  player with pipe_x = if(player.pipe_x = -75) then 600 else player.pipe_x - 5
}

let gravity t_delta player = 
  (* player |> velocity_change; *)
  match player.position with 
  | (x, y) -> 
    let is_jump' = if player.velocity = 0. then true else false in 
    { player with 
      position = (x, y +. (player.velocity *. t_delta) +. 
                     (0.5 *. gravity *. (t_delta**2.0)));
      velocity = velocity_change t_delta player;
      is_jump = is_jump'}

let jump player = 
  if player.is_jump = false then 
    {player with velocity =  player.velocity +. jump_v; is_jump = true}
  else 
    player 
