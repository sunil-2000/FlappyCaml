let gravity = -1.
let old_t = ref (Unix.gettimeofday())
let max_down = -2.
let jump_v = 7.

type t = {
  (* name : string;
     sprite : string; *)
  position : (float * float);
  velocity : float;
  (* orientation : float;
     mutable is_jump: bool; *)
  pipe_x : int
}

let create_t pos v = {
  position = pos;
  velocity = v;
  pipe_x = 600
}

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let get_pipe player = 
  player.pipe_x

let velocity_change player = 
  let t_delta = Unix.gettimeofday() -. !old_t in
  max (player.velocity +. (gravity *. t_delta)) max_down

let pipe_change player = {
  player with pipe_x = player.pipe_x - 5
}

let gravity player = 
  (* player |> velocity_change; *)
  let t_delta = Unix.gettimeofday()  -. !old_t in
  match player.position with 
  | (x, y) -> { player with position = 
                              (x, y +. 
                                  (player.velocity *. t_delta) +. 
                                  (0.5 *. gravity *. (t_delta**2.0)));
                            velocity = velocity_change player}

let jump player = 
  old_t := Unix.gettimeofday ();
  {player with velocity =  player.velocity +. jump_v}