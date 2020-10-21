(* need to constantly apply gravity, when jump is needed apply arbitrary 
   upwards force to player, which is counteracted by gravity until player starts 
   falling. 
   Position (y-values): y(n + 1) = v * t + y(n)
   Velocity: v(n + 1) = a * t + v(n) *)

let gravity = -9.8
let old_t = ref (Unix.gettimeofday())
let velocity_global = 1.0
let max_down = -5.
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
  | v -> player.velocity <- max (v +. (gravity *. t_delta)) max_down

(* let calc_player_pos player = 
   let x = velocity_change player in
   let velocity = player.velocity in
   (**let t_delta = 0.01 (**Unix.gettimeofday() -. !old_t in*) in*)
   match player.position with 
   | (x, y) -> player.position <- 
      (x, y +. (velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0))) *)

let calc_player_pos player = 
  player |> velocity_change;
  (**let t_delta = 0.01 (**Unix.gettimeofday() -. !old_t in*) in*)
  match player.position with 
  | (x, y) -> player.position <- 
      (x, y +. (player.velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0))) 


(* let y_delta player velocity = 
   let t_delta = Unix.gettimeofday() -. !old_t in 
   match player.position with 
   | (x, y) -> player.position  <- (x , y +. (velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0)));
    old_t := Unix.gettimeofday(); *)










