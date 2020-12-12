let gravity_global = -150.
(* let old_t = ref (Unix.gettimeofday()) *)
let max_down = -220.
let jump_v = 230.
let jump_v_run = 340.
(* let t_delta = 1.0 *)
(* let min = bottom_of_screen *)
let pipe_width = 73
let pipe_1_set = (250, 500)
let pipe_2_set = (337, 575)
let pipe_3_set = (141, 400)

let player_width = 50
let player_height = 50

let bottom_height = 100

let cactus_left_width = 24 
let cactus_mid_width = 22 
let cactus_width = 70 
let cactus_height = 117

type obs_rec = {
  obs_type : int option; 
  obs_x : int
}

type obstacle = Pipe of obs_rec | Cactus of obs_rec 

type t = {
  position : (float * float);
  velocity : float;
  can_jump : bool;
  collision : bool;
  score : int;
  score_updated : bool;
  highscore : int;
  obstacle : obstacle 
}

type player = t 
(* {obst_type = None; obst_x = 600} <- inital values *)
let create_obstacle obs_name obs_int obs_x = 
  match obs_name with 
  | "pipe" -> Pipe {obs_type = obs_int; obs_x = obs_x}
  | "cactus" -> Cactus {obs_type = obs_int; obs_x = obs_x}
  | _ -> failwith "create_obstacle"


let create pos v highscore obs_name obs_int obs_x = {
  position = pos;
  velocity = v;
  (* pipe_x = 600; *)
  can_jump = true;
  collision = false;
  (* pipe_type = Random.int 3; *)
  score = 0;
  score_updated = false;
  highscore = highscore;
  obstacle = create_obstacle obs_name obs_int obs_x
}

let set_can_jump player bool = 
  {player with can_jump = bool}

let set_obs_type player name  =
  match name, player.obstacle with
  | "cactus", (Pipe o | Cactus o)-> 
    let new_obs = Cactus {obs_type = Some (Random.int 1); obs_x = o.obs_x} in  (* 1 bc only 1 type cactus rn *)
    {player with obstacle = new_obs}
  | "pipe", (Pipe o | Cactus o) -> 
    let new_obs = Pipe {obs_type = Some (Random.int 3); obs_x = o.obs_x} in  
    {player with obstacle = new_obs}
  | _ -> failwith "set_obs_type"

let get_y player =
  match player.position with 
  | (x,y) -> y 

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let get_obs_x player = 
  match player.obstacle with
  | Pipe {obs_type = _ ; obs_x = x} -> x 
  | Cactus {obs_type = _ ; obs_x = x} -> x + cactus_left_width
  | _ -> failwith"cannot get obstacle x"

let get_pipe_type player = 
  match player.obstacle with 
  | Pipe {obs_type = Some x; obs_x = _} -> x
  | Cactus {obs_type = Some x; obs_x = _} -> x
  | _ -> failwith "obstacle not impl (get_pipe_type)"

let get_collision player = 
  player.collision

let get_score player =
  player.score

let get_score_updated player =
  player.score_updated

let get_highscore player = 
  max player.score player.highscore

let velocity_change t_delta player =  
  max (player.velocity +. (3. *. gravity_global *. t_delta)) max_down

let move_obs player = 
  match player.obstacle with 
  | Pipe { obs_type = Some i; obs_x = b} -> 
    if b = -75 then 
      {player with obstacle = Pipe { obs_type = Some i; obs_x = 600}}
    else  
      {player with obstacle = Pipe { obs_type = Some i; obs_x = b -5}}
  | Cactus { obs_type = Some i; obs_x = b} -> 
    if b = -75 then 
      {player with obstacle = Cactus { obs_type = Some i; obs_x = 600}}
    else  
      {player with obstacle = Cactus { obs_type = Some i; obs_x = b -5}}
  | _ -> failwith "move_obs"

let pipe_type_change player = 
  match player.obstacle with 
  | Pipe { obs_type = Some i; obs_x = b} -> 
    if b = -75 then 
      {player with obstacle = Pipe {obs_type = Some (Random.int 3); obs_x = b}}
    else 
      player 
  | Cactus _ -> player 
  | _ -> failwith "pipe_type_change"

let gravity_fly t_delta player = 
  match player.position with 
  | (x, y) -> 
    let can_jump' = if player.velocity = 0. then true else false in 
    { player with 
      position = (x, y +. (player.velocity *. t_delta) +. 
                     (0.5 *. gravity_global *. (t_delta**2.0)));
      velocity = velocity_change t_delta player;
      can_jump = can_jump'}

let gravity_run t_delta player = 
  match player.position with 
  | (x, y) -> 
    { player with 
      position = (x,max (y +. (player.velocity *. t_delta) +. 
                         (0.5 *. gravity_global *. (t_delta**2.0))) 100.);
      velocity = velocity_change t_delta player;
      can_jump = false}

let gravity t_delta player = 
  match player.obstacle with 
  | Pipe _ -> gravity_fly t_delta player
  | Cactus _ -> gravity_run t_delta player

let gravity_zero t_delta player = 
  match player.position with 
  | (x, y) -> 
    { player with 
      position = (x,y -. (player.velocity *. t_delta) +. 
                    (0.5 *. 150. *. (t_delta**2.0))) ;
      velocity = velocity_change t_delta player;
      can_jump = false}

let jump_aux player vel_value =  
  if player.can_jump then 
    {player with velocity = vel_value; can_jump = false}
  else 
    player 

let jump player = 
  match player.obstacle with 
  | Pipe _ -> jump_aux player jump_v 
  | Cactus _ -> jump_aux player jump_v_run

(* [pipe_chooser player] matches pipe_type and returns the bottom y value of the
   top pipe and the top y value of the bottom pipe in the form of a tuple *)
let pipe_chooser player=
  match player.obstacle with
  | Pipe {obs_type = Some i; obs_x = _ } -> 
    begin
      match i with 
      | 0 -> pipe_1_set
      | 1 -> pipe_2_set
      | 2 -> pipe_3_set
      | _ -> failwith "more"
    end 
  | Cactus _ -> failwith "no pipes for cactus <- pipe chooser" 
  | _ -> failwith "pipe_chooser"

let get_player_x player = 
  match player.position with 
  | (x, y) -> int_of_float x 

let get_player_y player = 
  match player.position with 
  | (x, y) -> int_of_float y

let collision player =
  let obs_x = get_obs_x player in 
  let left_boundary = obs_x - player_width in
  let right_boundary = obs_x + pipe_width in 
  let player_x = get_player_x player in 

  if player_x > left_boundary && player_x < right_boundary then 
    let player_y =  get_player_y player in 
    match pipe_chooser player with 
    | (bottom, top) -> 
      if player_y <= bottom + bottom_height || player_y + player_height >= top then 
        {player with collision = true}
      else 
        player 
  else 
    player 

let collision_run player = 
  let left_boundary = get_obs_x player  in
  let right_boundary = get_obs_x player + cactus_mid_width in 
  let player_x = get_player_x player in
  if player_x > left_boundary && player_x < right_boundary then 
    let player_y = get_player_y player in 
    if player_y <= bottom_height + 117 then 
      {player with collision = true}
    else 
      player
  else 
    player

let score_update player =
  (* let left_boundary = player.pipe_x - player_width in *)
  let right_boundary = get_obs_x player + pipe_width in 
  let player_x = get_player_x player in 
  if player_x > right_boundary && player.score_updated = false then 
    {player with score = player.score + 1; score_updated = true}
  else if player_x < right_boundary then
    {player with score_updated = false}
  else
    player

let update t_delta player  = 
  if player.can_jump then
    (* jumps with gravity applied after, then apply pipe change *)
    jump player |> 
    gravity t_delta |> 
    move_obs |> 
    pipe_type_change |> 
    collision |> 
    score_update
  else 
    gravity t_delta player |> 
    move_obs |> 
    pipe_type_change |> 
    collision |> 
    score_update

let update_run t_delta player =
  if player.can_jump then
    (* jumps with gravity applied after, then apply pipe change *)
    jump player 
    |> gravity t_delta 
    |> move_obs 
    |> collision_run
    |> score_update
  else 
    gravity t_delta player 
    |> move_obs
    |> collision_run 
    |> score_update

let update_togo t_delta player = 
  gravity_zero t_delta player 

let update_torun t_delta player = 
  gravity t_delta player 
