let gravity_global = -150.
(* let old_t = ref (Unix.gettimeofday()) *)
let max_down = -220.
let jump_v = 230.
(* let t_delta = 1.0 *)
(* let min = bottom_of_screen *)
let pipe_width = 73
let pipe_1_set = (250, 500)
let pipe_2_set = (337, 575)
let pipe_3_set = (141, 400)

let player_width = 50
let player_height = 50

let bottom_height = 100

type t = {
  (* name : string;
     sprite : string; *)
  position : (float * float);
  velocity : float;

  pipe_x : int;
  game_over : bool;
  can_jump : bool;
  pipe_type : int;
  collision : bool;
  score : int;
  score_updated : bool;
}

let create pos v = {
  position = pos;
  velocity = v;
  pipe_x = 600;
  game_over = false;
  can_jump = true;
  collision = false;
  pipe_type = Random.int 3;
  score = 0;
  score_updated = false;
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

let get_collision player = 
  player.collision

let get_score player =
  player.score

let get_score_updated player =
  player.score_updated

let velocity_change t_delta player =  
  max (player.velocity +. (3. *. gravity_global *. t_delta)) max_down

let pipe_change player = {
  player with pipe_x = if player.pipe_x = -75 then 600 else player.pipe_x - 5
}

let pipe_type_change player = {
  player with pipe_type = if player.pipe_x = -75 then Random.int 3 else player.pipe_type
}

let run_pipe_choose player = {
  player with pipe_type = 1
}

let gravity t_delta player = 
  (* player |> velocity_change; *)
  match player.position with 
  | (x, y) -> 
    let is_jump' = if player.velocity = 0. then true else false in 
    { player with 
      position = (x, y +. (player.velocity *. t_delta) +. 
                     (0.5 *. gravity_global *. (t_delta**2.0)));
      velocity = velocity_change t_delta player;
      can_jump = is_jump'}

(** [gravity_run n p] applies gravity to player during the Run game state. *)
let gravity_run t_delta player = 
  (* player |> velocity_change; *)
  match player.position with 
  | (x, y) -> 
    { player with 
      position = (x,max (y +. (player.velocity *. t_delta) +. 
                         (0.5 *. gravity_global *. (t_delta**2.0))) 100.);
      velocity = velocity_change t_delta player;
      can_jump = false}

let jump player = 
  if player.can_jump then 
    {player with velocity = jump_v; can_jump = false}
  else 
    player 

(* [pipe_chooser player] matches pipe_type and returns the bottom y value of the
   top pipe and the top y value of the bottom pipe in the form of a tuple *)
let pipe_chooser player=
  match player.pipe_type with
  | 0 -> pipe_1_set
  | 1 -> pipe_2_set
  | 2 -> pipe_3_set
  | _ -> failwith "more"

let get_player_x player = 
  match player.position with 
  | (x, y) -> int_of_float x 

let get_player_y player = 
  match player.position with 
  | (x, y) -> int_of_float y

let collision player = 
  let left_boundary = player.pipe_x - player_width in
  let right_boundary = player.pipe_x + pipe_width in 
  let player_x = get_player_x player in 

  if player_x > left_boundary && player_x < right_boundary then 
    let player_y =  get_player_y player in 
    match pipe_chooser player with 
    | (bottom, top)-> 
      if player_y <= bottom + bottom_height || player_y + player_height >= top then 
        {player with collision = true}
      else 
        player 
  else 
    player 

let score_update player =
  (* let left_boundary = player.pipe_x - player_width in *)
  let right_boundary = player.pipe_x + pipe_width in 
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
    pipe_change |> 
    pipe_type_change |> 
    collision |> 
    score_update
  else 
    gravity t_delta player |> 
    pipe_change |> 
    pipe_type_change |> 
    collision |> 
    score_update

let update_run t_delta player =
  if player.can_jump then
    (* jumps with gravity applied after, then apply pipe change *)
    jump player 
    |> gravity_run t_delta 
    |> pipe_change
    |> run_pipe_choose
    |> collision
    |> score_update
  else 
    gravity_run t_delta player 
    |> pipe_change
    |> run_pipe_choose
    |> collision
    |> score_update
