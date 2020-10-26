(* need to constantly apply gravity, when jump is needed apply arbitrary 
   upwards force to player, which is counteracted by gravity until player starts 
   falling. 
   Position (y-values): y(n + 1) = v * t + y(n)
   Velocity: v(n + 1) = a * t + v(n) *)

let gravity = -0.5
let old_t = ref (Unix.gettimeofday())
let max_down = -1.5
let jump_v = 10.
(* let t_delta = 1.0 *)
(* let min = bottom_of_screen *)

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

(* let set_velocity player v = 
   player.velocity <- v *)

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

(*   let set_position player p =
     player.position <- p *)
let velocity_change player = 
  let t_delta = 0.33 in    
  max (player.velocity +. (gravity *. t_delta)) max_down


let gravity player = 
  (* player |> velocity_change; *)
  let t_delta = Unix.gettimeofday() -. !old_t in
  match player.position with 
  | (x, y) -> { position = 
                  (x, y +. (player.velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0)));
                velocity = velocity_change player}

let jump player = 
  {player with velocity =  jump_v}










