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
}

let create_t pos v = {
  position = pos;
  velocity = v
}

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let velocity_change player = 
  let t_delta = Unix.gettimeofday() -. !old_t in
  max (player.velocity +. (gravity *. t_delta)) max_down

let gravity player = 
  (* player |> velocity_change; *)
  let t_delta = Unix.gettimeofday()  -. !old_t in
  match player.position with 
  | (x, y) -> { position = 
                  (x, y +. 
                      (player.velocity *. t_delta) +. 
                      (0.5 *. gravity *. (t_delta**2.0)));
                velocity = velocity_change player}

let jump player = 
  old_t := Unix.gettimeofday ();
  {player with velocity =  player.velocity +. jump_v}