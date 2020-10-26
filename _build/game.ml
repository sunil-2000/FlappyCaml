let gravity = -1.
let old_t = ref (Unix.gettimeofday())
<<<<<<< HEAD
let max_down = -1.5
let jump_v = 10.
(* let t_delta = 1.0 *)
(* let min = bottom_of_screen *)
=======
let max_down = -2.
let jump_v = 7.
>>>>>>> b0cd2a3fd5ffa3394e89f9be7fd485b755a5fe53

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
<<<<<<< HEAD
  let t_delta = 0.33 in    print_string "t_delta: ";
  printplayer.velocity +. (gravity *. t_delta)  print_flo
    loat player.velocity +. (gravity *. t_delta)
  ;(* let calc_player_pos player = 
      let x = velocity_change player in
      let velocity = player.velocity in
      (**let t_delta = 0.01 (**Unix.gettimeofday() -. !old_t in*) in*)
      match player.position with 
      | (x, y) -> player.position <- 
        (x, y +. (velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0))) *)

  let gravity player = 
    (* player |> velocity_change; *)
    let t_delta = Unix.gettimeofday() -. !old_t in
    match player.position with 
    | (x, y) -> { position = 
                    (x, y +. (player.velocity *. t_delta) +. (0.5 *. gravity *. (t_delta**2.0)));
                  velocity = velocity_change player}

                  print_float (player.velocity +. (gravity *. t_delta));                
      let jump player = 
        {player with velocity =  jump_v}










=======
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
>>>>>>> b0cd2a3fd5ffa3394e89f9be7fd485b755a5fe53
