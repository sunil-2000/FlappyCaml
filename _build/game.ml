let gravity = -150.
(* let old_t = ref (Unix.gettimeofday()) *)
let max_down = -220.
let jump_v = 230.
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
  can_jump : bool;
  pipe_type : int 
}

let create pos v = {
  position = pos;
  velocity = v;
  pipe_x = 600;
  game_over = false;
  can_jump = true;
  pipe_type = 0
}

let is_gameover t = 
  t.game_over

let set_can_jump player bool = 
  {player with can_jump = bool}

let get_y player =
  match player.position with 
  | (x,y) -> y 

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let get_pipe player = 
  player.pipe_x

let get_pipe_type player = 
  player.pipe_type

let velocity_change t_delta player =  
  max (player.velocity +. (3. *. gravity *. t_delta)) max_down

let pipe_change player = {
  player with pipe_x = if player.pipe_x = -75 then 600 else player.pipe_x - 5
}

let pipe_type_change player = {
  player with pipe_type = if player.pipe_x = -75 then Random.int 3 else player.pipe_type
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
      can_jump = is_jump'}

let jump player = 
  if player.can_jump then 
    {player with velocity = jump_v; can_jump = false}
  else 
    player 


let update t_delta player  = 
  if player.can_jump then
    (* jumps with gravity applied after, then apply pipe change *)
    jump player 
    |> gravity t_delta
    |> pipe_change
    |> pipe_type_change 
  else 
    gravity t_delta player |> pipe_change |> pipe_type_change

