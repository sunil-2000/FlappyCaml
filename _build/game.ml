(*****************************GLOBAL VARIABLES FOR GAME************************)
let gravity_global = -150.

let max_down = -220.
let jump_v = 230.
let jump_v_run = 340.

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

let normal_obs_move = 5
let slow_obs_move = 1

let game_width = 600 
let min_pwr = 200
let max_pwr = 500
let mushroom_w = 35
let mushroom_h = 34
let mushroom_w_buffer = 2 
let mushroom_h_buffer = 2 

let player_width = 50
let player_height = 50 

let previous_power_score = ref 0 

let bomb_drop = 10 
let bomb_x_int = 3
let bomber_h = 500
let b_interval = 180 
(******************************************************************************)
type obs_rec = {
  obs_type : int option; 
  obs_x : int;
  obs_y : int
}

type pwr_rec = {
  x : int ;
  y : int 
}

type obstacle = 
  | Pipe of obs_rec 
  | Cactus of obs_rec 

type powerup = Invincible of pwr_rec | Slow of pwr_rec | None

type bomb = Bomb of int * int * bool | None 

type bomb_rec = {
  bombs : bomb list; 
  bomber_x : int;
}

type t = {
  position : (float * float);
  velocity : float;
  can_jump : bool;
  collision : bool;
  score : int;
  score_updated : bool;
  highscore : int;
  obstacle : obstacle;
  bomb : bomb_rec;
  powerup : powerup;
  pwr_active : bool; 
}

type player = t 

(* [create_obstacle] creates a pipe or cactus object based on the obs_name
   passed, type of obstacle, and initial position*)
let create_obstacle obs_name obs_int obs_x obs_y = 
  match obs_name with 
  | "pipe" -> Pipe {obs_type = obs_int; obs_x = obs_x; obs_y = obs_y}
  | "cactus" -> Cactus {obs_type = obs_int; obs_x = obs_x; obs_y = obs_y}
  | _ -> failwith "create_obstacle"

(* [create] takes in a position (float * float), velocity (int), highscore (int)
   , obstacle name (string), obstacle type (int), x (int), and y (int) and 
   returns a player type with position and velocity specified and initializes 
   all the fields needed to create a game *)
(* set obs_y to 0 if pipe or cactus *)
let create pos v highscore obs_name obs_int obs_x obs_y = {
  position = pos;
  velocity = v;
  (* pipe_x = 600; *)
  can_jump = true;
  collision = false;
  (* pipe_type = Random.int 3; *)
  score = 0;
  score_updated = false;
  highscore = highscore;
  obstacle = create_obstacle obs_name obs_int obs_x obs_y;
  powerup = None;
  bomb = {bombs = []; bomber_x = 600};
  pwr_active = false;
}
(*****************************GETTERS******************************************)
let get_player_x player = 
  match player.position with 
  | (x, y) -> int_of_float x 

let get_player_y player = 
  match player.position with 
  | (x, y) -> int_of_float y

let get_collision player = 
  player.collision

let get_score player =
  player.score

let get_score_updated player =
  player.score_updated

let get_highscore player = 
  max player.score player.highscore

let get_velocity player = 
  player.velocity 

let get_position player =
  player.position 

let get_pwr_pos player =
  match player.powerup with
  | Invincible {x = x ; y = y} -> (x, y)
  | Slow {x = x ; y = y} -> (x, y) 
  | None -> (0, 0) 

let get_obs_x player = 
  match player.obstacle with
  | Pipe {obs_type = _ ; obs_x = x} -> x 
  | Cactus {obs_type = _ ; obs_x = x} -> x + cactus_left_width

let get_obs_type player = 
  match player.obstacle with 
  | Pipe {obs_type = Some x; obs_x = _} -> x
  | Cactus {obs_type = Some x; obs_x = _} -> x
  | _ -> failwith "obstacle not impl (get_pipe_type)"

let get_bomber_x player = 
  player.bomb.bomber_x

let get_bombs_list player = 
  player.bomb.bombs

let get_bomb_rec player = 
  player.bomb 
(******************************************************************************)
(***********************************SETTERS************************************)
let set_score int player = 
  {player with score = int}

let set_collision bool player = 
  {player with collision = bool}

let set_can_jump player bool = 
  {player with can_jump = bool}

let set_obs_x x player = 
  match player.obstacle with 
  | Pipe {obs_type = Some i; obs_x = b; obs_y = c} -> 
    {player with obstacle = Pipe {obs_type = Some i; obs_x = x; obs_y = c}}
  | Cactus { obs_type = Some i; obs_x = b; obs_y = c} -> 
    {player with obstacle = Cactus {obs_type = Some i; obs_x = x; obs_y = c}}
  | _ -> failwith"set_obs_x"

let set_obs_type name player  =
  Random.self_init ();
  match name, player.obstacle with
  | "cactus", (Pipe o | Cactus o)-> 
    let new_obs = 
      Cactus {obs_type = Some (Random.int 1); obs_x = o.obs_x; 
              obs_y = o.obs_y} in  (* 1 bc only 1 type cactus rn *)
    {player with obstacle = new_obs}
  | "pipe", (Pipe o | Cactus o) -> 
    let new_obs = 
      Pipe {obs_type = Some (Random.int 3); obs_x = o.obs_x; obs_y = o.obs_y} in  
    {player with obstacle = new_obs}
  | _ -> failwith "set_obs_type"

let set_powerup name x y player = 
  match name with 
  | "slow" -> let pwrup = Slow {x = x; y = y} in 
    {player with powerup = pwrup}
  | "invincible" -> let pwrup = Invincible {x = x; y = y} in 
    {player with powerup = pwrup}
  | _ -> {player with powerup = None}

let set_pwr_active player bool = 
  {player with pwr_active = bool}

(* [set_pwr_none player] is used to set the powerup field to None and 
   pwr_active field to false. This function is called when the powerup should 
   stop being applied to the player. *)
let set_pwr_none player = 
  {player with powerup = None; pwr_active = false }
(******************************************************************************)
(*******************GAME MECHANICS FUNCTIONS***********************************)

let velocity_change t_delta player =  
  max (player.velocity +. (3. *. gravity_global *. t_delta)) max_down

let move_obs d player = 
  match player.obstacle with 
  | Pipe { obs_type = Some i; obs_x = b} -> 
    if b <= -75 then 
      {player with obstacle = Pipe { obs_type = Some i; 
                                     obs_x = 600; obs_y = 0}}
    else  
      {player with obstacle = Pipe { obs_type = Some i; 
                                     obs_x = b - d; obs_y = 0}}
  | Cactus { obs_type = Some i; obs_x = b} -> 
    if b <= -75 then 
      {player with obstacle = Cactus { obs_type = Some i; 
                                       obs_x = 600; obs_y = 0}}
    else  
      {player with obstacle = Cactus { obs_type = Some i; 
                                       obs_x = b - d; obs_y = 0}}
  | _ -> failwith "move_obs"

let pipe_type_change player = 
  Random.self_init ();
  match player.obstacle with 
  | Pipe { obs_type = Some i; obs_x = b} -> 
    if b = -75 then 
      {player with obstacle = Pipe {obs_type = Some (Random.int 3); 
                                    obs_x = b; obs_y = 0}}
    else 
      player 
  | Cactus _ -> player 
  | _ -> failwith "pipe_type_change"

(* [gravity_fly t_delta player] is the main gravity function used when state
   = Go (fly) or Bomb *)
let gravity_fly t_delta player = 
  match player.position with 
  | (x, y) -> 
    let can_jump' = if player.velocity = 0. then true else false in 
    { player with 
      position = (x, y +. (player.velocity *. t_delta) +. 
                     (0.5 *. gravity_global *. (t_delta**2.0)));
      velocity = velocity_change t_delta player;
      can_jump = can_jump'}

(* [gravity_run t_delta player] is a gravity function used to bring the player
   to the ground. It is used when state = ToRun and the player, should be dropped
   to the ground *)
let gravity_run t_delta player = 
  match player.position with 
  | (x, y) -> 
    { player with 
      position = (x,max (y +. (player.velocity *. t_delta) +. 
                         (0.5 *. gravity_global *. (t_delta**2.0))) 100.);
      velocity = velocity_change t_delta player;
      can_jump = false}

(* [gravity_zero t_delta player] emulates a zero gravity state, which is used
   in the ToGo state *)
let gravity_zero t_delta player = 
  match player.position with 
  | (x, y) -> 
    { player with 
      position = (x,y -. (player.velocity *. t_delta) +. 
                    (0.5 *. 150. *. (t_delta**2.0))) ;
      velocity = velocity_change t_delta player;
      can_jump = false}

(* [gravity t_delta player] uses the appropriate gravity function when state is
   either Run or Go *)
let gravity t_delta player = 
  match player.obstacle with 
  | Pipe _ -> gravity_fly t_delta player
  | Cactus _ -> gravity_run t_delta player

(* [jump_aux player vel_value] is a helper function for [jump] that sets the 
   player's velocity to [vel_value] if the player.can_jump field is true. *)
let jump_aux player vel_value =  
  if player.can_jump then 
    {player with velocity = vel_value; can_jump = false}
  else 
    player 

(* [jump player] is a function that is responsible for the jump mechanice. It 
   applies the jump if player.can_jump = true *)
let jump player = 
  if player.can_jump then 
    match player.obstacle with 
    | Pipe _  -> jump_aux player jump_v 
    | Cactus _ -> jump_aux player jump_v_run
  else 
    player 

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

(* [collision player] returns {player with collision = true} if the player 
   hits a pipe, during the Go state, otherwise it returns the same player *)
let collision player =
  let obs_x = get_obs_x player in 
  let left_boundary = obs_x - player_width in
  let right_boundary = obs_x + pipe_width in 
  let player_x = get_player_x player in 

  if player_x > left_boundary && player_x < right_boundary then 
    let player_y =  get_player_y player in 
    match pipe_chooser player with 
    | (bottom, top) -> 
      if player_y <= bottom + bottom_height || 
         player_y + player_height >= top then 
        {player with collision = true}
      else 
        player 
  else 
    player 

(* [collision_run player] returns {player with collision = true} if the player 
   hits a cactus, during the Run state, otherwise it returns the same player *)
let collision_run player = 
  let left_boundary = get_obs_x player in
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

(* [score_update player] appropriately updates the players score, +1 to the 
   score when a player passes an obstacle *)
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

(* [collision_reset player] sets the collision to false. It is 
   used for the invincibility powerup *)
let collision_reset player =
  {player with collision = false}

(* [generate_random bound1 bound2] returns a random int between bound1
   and bound2 *)
let generate_random bound1 bound2 =
  Random.self_init ();
  let diff = (max bound1 bound2) - (min bound1 bound2) in 
  Random.int diff + (min bound1 bound2)

(* [create_powerup pwr_name] creates the appropriate powerup object in the 
   game depending on [pwr_name] *)
let create_powerup pwr_name = 
  match pwr_name with 
  | "invincible" -> 
    Invincible {x = 900; y = generate_random min_pwr max_pwr}
  | "slow" -> 
    Slow {x = 900 ; y = generate_random min_pwr max_pwr}
  | "none" -> None 
  | _ -> failwith "create_obstacle, invalid string input (not a powerup)"

(* [generate_powerup player] randomly generates a powerup object in the game.
   A powerup is not generated if one is already active. *)
let generate_powerup player = 
  Random.self_init ();
  if player.powerup = None then 
    match Random.int 3 with 
    | 0 -> {player with powerup = None}
    | 2 -> 
      if player.pwr_active then 
        {player with powerup = None} 
      else 
        {player with powerup = create_powerup "slow"}
    | 1 -> 
      if player.pwr_active then 
        {player with powerup = None}
      else 
        {player with powerup = create_powerup "invincible"} 
    | _ -> failwith "generate_powerup"
  else 
    player 

(* [powerup_change player] sets the powerup field to None and pwr_active field
   to false if the player has passed two obstacles (score incremented by 2) *)
let powerup_change player = 
  if player.score - !previous_power_score = 2 then 
    {player with powerup = None; pwr_active = false}
  else 
    player 

(* [move_powerup player] moves the powerup object appropriately. If active is true
   then return None as powerup *)
let move_powerup player = 
  match player.powerup, player.pwr_active with 
  | None, _ -> {player with powerup = None}
  | Slow {x = x; y = y}, false -> 
    {player with powerup = Slow {x = x - normal_obs_move; y = y}}
  | Invincible {x = x; y = y}, false -> 
    {player with powerup = Invincible {x = x - normal_obs_move; y = y}}
  | Invincible {x = x; y = y}, true-> {player with powerup = Invincible {x = 0; y = 0}}
  | Slow {x =x; y =y}, true -> {player with powerup = Slow {x = 0; y = 0}}

(* [rectangle_collision player] takes two rectangular objects and returns
   true if they have collided, false otherwise *)
let rectangle_collision player w1 h1 w2 h2  =
  let rect1x = get_player_x player in 
  let rect1y = get_player_y player in 

  let rect2x = fst(get_pwr_pos player) in 
  let rect2y = snd (get_pwr_pos player) in 

  rect1x < rect2x + w2 && 
  rect1x + w1 > rect2x 
  && rect1y < rect2y + h2 && 
  rect1y + h1 > rect2y   

(* [powerup_collision player] return player.powerup with the active field as
   true if the player has collided with a powerup object, otherwise it returns
   the player unchanged. *)
let powerup_collision w1 h1 w2 h2 player = 
  if player.pwr_active <> true && player.powerup <> None then 
    match rectangle_collision player w1 h1 w2 h2 with 
    | true -> 
      previous_power_score := player.score; 
      {player with pwr_active = true}
    | false -> player
  else 
    player 

(********************************BOMB STATE************************************)

let rec generate_aux (lst : bomb list) mult  = 
  Random.self_init ();
  if List.length lst < 3 then 
    match lst, 1 with 
    | [] , 1 -> 
      let lst' = Bomb (game_width - b_interval * mult, 500, false) :: lst in 
      generate_aux lst' (mult + 1)
    | h :: t, 1 -> 
      let lst' = Bomb (game_width - b_interval * mult, 500, false) :: lst in 
      generate_aux lst' (mult + 1)
    | [] , 0 -> 
      let lst' = None :: lst in 
      generate_aux lst' (mult + 1)
    | h :: t, 0 -> 
      let lst' = None :: lst in 
      generate_aux lst' (mult + 1)
    | _, _ -> failwith "[generate_aux] failed in game"
  else 
    lst 

let move_bomber player = 
  if player.bomb.bomber_x = 600 then 
    {player with
     bomb = 
       {bombs = generate_aux [] 1; 
        bomber_x = player.bomb.bomber_x - 1 }}
  else if player.bomb.bomber_x = -240 then 
    {player with score = player.score + 5; 
                 bomb = 
                   {bombs = player.bomb.bombs; 
                    bomber_x = player.bomb.bomber_x - 1}}
  else 
    {player with bomb = {bombs = player.bomb.bombs; 
                         bomber_x = player.bomb.bomber_x - 1}} 

let rec dropping_bombs lst = 
  match lst with 
  | h :: t -> 
    begin 
      match h with  
      | Bomb (x, y, bool) -> 
        if bool then 
          Bomb (x - 1, y - 1, bool) :: dropping_bombs t 
        else 
          Bomb (x, y, bool) :: dropping_bombs t 
      | None -> None :: dropping_bombs t
    end 
  | [] -> []

let rec make_true bomb_list player = 
  match bomb_list with 
  | [] -> []
  | h :: t -> begin
      match h with 
      | None -> None :: make_true t player
      | Bomb (x, y, b) -> begin
          if x >= player.bomb.bomber_x then 
            Bomb (x, y, true) :: make_true t player 
          else 
            Bomb (x, y, b) :: make_true t player
        end
    end


let drop_bomb player = 
  if (game_width - player.bomb.bomber_x) mod b_interval = 0 && 
     player.bomb.bomber_x > 0 then 
    {player with bomb = {bombs = make_true player.bomb.bombs player; 
                         bomber_x = player.bomb.bomber_x }}
  else 
    {player with bomb = {bombs = dropping_bombs player.bomb.bombs; 
                         bomber_x = player.bomb.bomber_x}}


let bomb_collision player x1 y1 = 
  let rect1x = get_player_x player in 
  let rect1y = get_player_y player in 

  let rect2x = x1 in 
  let rect2y = y1 in 

  rect1x < rect2x + 30 && 
  rect1x + 50 > rect2x 
  && rect1y < rect2y + 30 && 
  rect1y + 50 > rect2y   

let collision_ground player =
  let player_y =  get_player_y player in 
  if player_y <= bottom_height then 
    {player with collision = true}
  else 
    player 

let rec check_bomb_collision  player lst = 
  match lst with 
  | h :: t -> begin
      match h with
      | None -> check_bomb_collision player t
      | Bomb (x, y, b) ->  
        if b && bomb_collision player x y then 
          true 
        else 
          check_bomb_collision player t
    end
  | [] -> false

(* [collision_bomb player] returns {player with collision = true} if the player 
   hits a bomb, during the Bomb state, otherwise it returns the same player *)
let collision_bomb player = 
  if check_bomb_collision player player.bomb.bombs then {
    player with collision = true} 
  else
    player

(******************************************************************************)

(************************GAME UPDATE FUNCTIONS*********************************)

(* [update t_delta player state] is responsible for executing the appropriate 
   game update, depending on the current state of the game *)
let rec update t_delta player state_string = 
  match state_string with 
  | "go" -> update_fly t_delta player 
  | "run" -> update_run t_delta player
  | "togo" -> update_togo t_delta player
  | "torun" -> update_torun t_delta player
  | "tobomb" -> update_tobomb t_delta player 
  | "death" -> update_death t_delta player
  | "bomb" -> update_bomb t_delta player
  | _ -> failwith "update not implemented in game.ml"

(* [update_fly_aux t_delta player] is a helper for [update_fly] *)
and update_fly_aux t_delta player obs_move = 
  jump player 
  |> gravity t_delta 
  |> move_obs obs_move 
  |> pipe_type_change 
  |> collision 
  |> powerup_change 
  |> generate_powerup 
  |> move_powerup 
  |> powerup_collision player_width player_height mushroom_w mushroom_h
  |> score_update

(* [update_fly t_delta player] updates player when state = fly (go). [update_fly]
   gives the appropriate update if the player has a powerup *)
and update_fly t_delta player = 
  match player.powerup, player.pwr_active with 
  | Slow _ , true -> 
    update_fly_aux t_delta player slow_obs_move
  | Invincible _, true -> 
    update_fly_aux t_delta player normal_obs_move |> collision_reset 
  | None, _ | _ -> 
    update_fly_aux t_delta player normal_obs_move


and update_run_aux t_delta player obs_move = 
  jump player 
  |> gravity t_delta 
  |> move_obs obs_move
  |> collision_run
  |> generate_powerup 
  |> move_powerup
  |> powerup_collision player_width player_height mushroom_w mushroom_h
  |> powerup_change 
  |> score_update

(* [update_run t_delta player] updates player when state = run. *)
and update_run t_delta player =
  match player.powerup, player.pwr_active with 
  | Slow _ , true ->  
    update_run_aux t_delta player slow_obs_move
  | Invincible _, true -> 
    update_run_aux t_delta player normal_obs_move |> collision_reset 
  | None, _ | _ -> 
    update_run_aux t_delta player normal_obs_move 

(* [update_totun t_delta player] updates the player and obstacles while in 
   state ToRun. Obstacles are generated offscreen at -100 *)
and update_torun t_delta player = 
  set_pwr_none player
  |> gravity_run t_delta 
  |> set_obs_x (-100)

(* [update_togo t_delta player] updates the player and obstacles while in 
   state ToGo. Obstacles are generated offscreen at -100 *)
and update_togo t_delta player = 
  set_pwr_none player
  |>gravity_zero t_delta 
  |> set_obs_x (-100)

(* [update_tobomb t_delta player] updates the player when state = tobomb *)
and update_tobomb t_delta player = 
  set_pwr_none player 
  |> gravity_zero t_delta 
  |> move_obs normal_obs_move

(* [update_death t_delta player] updates the player when state = death *)
and update_death t_delta player = 
  gravity_fly t_delta player 

(* [update_bomb t_delta player] updates the player when state = bomb *)
and update_bomb t_delta player = 
  jump player 
  |> gravity_fly t_delta
  |> move_bomber  
  |> drop_bomb 
  |> collision_bomb 
  |> collision_ground

(***************************DEBUG FUNCTIONS / PRINTERS*************************)

let string_of_obstacle player = 
  match player.obstacle with 
  | Pipe _ -> "pipe"
  | Cactus _ -> "cactus"

let string_of_velocity player = 
  string_of_float player.velocity 

let string_of_score player = 
  string_of_int player.score

let string_of_highscore player = 
  string_of_int player.highscore 

let string_of_powerup player = 
  match player.powerup with 
  | Invincible _ -> "invincible"
  | None -> "none"
  | Slow _ -> "slow"

let int_of_powerup player = 
  match player.powerup with 
  | None  -> -1
  | Invincible _ -> 0
  | Slow _ -> 1

let get_pwr_active player =
  player.pwr_active

let string_of_bomb bomb = 
  match bomb with 
  | None -> "None"
  | Bomb (x, y, b) -> "[Bomb: (" ^ string_of_int x ^ ", " ^ 
                      string_of_int y ^ "); " ^ string_of_bool b ^ "]"

let rec pp_bomb_lst_aux lst = 
  match lst with 
  | h :: t -> string_of_bomb h ^ pp_bomb_lst_aux t 
  | [] -> ""

let pp_bomb_lst player = 
  pp_bomb_lst_aux player.bomb.bombs 

