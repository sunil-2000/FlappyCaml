open State 
open Graphics 
open Game 
open OUnit2

(***************************State Module Tests*********************************)
let start_state = State.make_state () 
let go_state = State.make_state () 
               |> State.set_go 
let run_state = State.make_state () 
                |> State.set_run 
let togo_state = State.make_state () 
                 |> State.set_togo 
let torun_state = State.make_state () 
                  |> State.set_torun 
let death_state = State.make_state () 
                  |> State.set_death

let go_player_alive = Game.create (200., 400.) 10. 7 "pipe" (Some 2) 400 100
let go_player_floor = Game.create (200., 99.) 10. 7 "pipe" (Some 2) 400 100
let go_player_pipe = Game.create (200., 150.) 10. 7 "pipe" (Some 2) 200 100
                     |> Game.set_collision true
let go_player_transition = 
  Game.create (200., 400.) 10. 4 "pipe" (Some 2) 400 100
  |> Game.set_score 4

let run_player_alive = Game.create (200., 100.) 0. 7 "cactus" (Some 1) 400 100
let run_player_dead = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 175 100
                      |> Game.set_collision true
let run_player_transition = 
  Game.create (200., 100.) 10. 7 "cactus" (Some 1) 400 100
  |> Game.set_score 4

let togo_player_yes = Game.create (200., 400.) 10. 7 "pipe" (Some 2) 400 100
let togo_player_no = Game.create (200., 349.) 10. 7 "pipe" (Some 2) 400 100

let torun_player_yes = Game.create (200., 97.) 10. 7 "cactus" (Some 1) 400 100
let torun_player_no = Game.create (200., 101.) 10. 7 "cactus" (Some 1) 400 100

let dead_player_gameover = 
  Game.create (200., -50.) 10. 7 "cactus" (Some 1) 400 100 
let dead_player_nogameover = 
  Game.create (200., -49.) 10. 7 "cactus" (Some 1) 400 100

let check_test 
    (name: string)
    (state: State.t)
    (player: Game.t)
    (expected_output: string) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (State.check state player 
                                  |> State.string_of_state)

let state_tests = [
  check_test "state: fly-state, state of player: alive player"
    go_state go_player_alive "go";
  check_test "state: fly-state, state of player: player hits floor" 
    death_state go_player_floor "death";
  check_test "state: fly-state, state of player: player hits obstacle" 
    death_state go_player_pipe "death";
  check_test "fly-state, player transitions to ToRun" 
    torun_state go_player_transition "torun";
  check_test "state: run-state, state of player: alive player" 
    run_state run_player_alive "run";
  check_test "run-state, dead player" run_state run_player_dead "death";
  check_test "ToGo transition ok" togo_state togo_player_yes "go";
  check_test "ToGo transition not ok" togo_state togo_player_no "togo";
  check_test "ToRun transition ok" torun_state torun_player_yes "run";
  check_test "ToRun transition not ok" torun_state torun_player_no "torun";
  check_test "transition to Gameover" 
    death_state dead_player_gameover "gameover";
  check_test "no transition to gameover" 
    death_state dead_player_nogameover "death";
]
(******************************************************************************)

(*****************************Game Module Tests********************************)
(* Update is tested by passing a player through Game.update and then comparing 
   against the updated fields using getters*)

let update_player player state_string = 
  Game.update 0.01 player state_string

let update_test
    (name: string)
    (player: Game.t)
    (state_string: string)
    (t_delta: float)
    (getter)
    (expected_output) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (Game.update t_delta player state_string 
                                  |> getter) 

let jump_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: float) : test = 
  update_test name player state_string 0.01 Game.get_velocity expected_output

let gravity_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string 0.1 Game.get_player_y expected_output 

let move_obs_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string 0.1 Game.get_obs_x expected_output

let pipe_type_change_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test =
  update_test name player state_string 0.1 Game.get_pipe_type expected_output  

let collision_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string 0.01 Game.get_collision expected_output

let powerup_collision_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string 0.01 Game.get_pwr_active expected_output

let move_powerup_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int * int) : test = 
  update_test name player state_string 0.1 Game.get_pwr_pos expected_output

let score_update_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string 0.1 Game.get_score expected_output

let score_updated_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string 0.1 
    Game.get_score_updated expected_output

let move_bomber_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string 0.1 Game.get_bomber_x expected_output

let string_of_state_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (t_delta: float)
    (getter)
    (expected_output) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (Game.update t_delta player state_string 
                                  |> getter) 

let fly_can_jump = go_player_alive
let fly_no_jump = Game.set_can_jump fly_can_jump false
let no_vel = Game.create (200., 400.) 0. 5 "pipe" (Some 1) 400 100 
let fly_no_vel = Game.set_can_jump no_vel false
let pipe_offs = Game.create (200., 400.) 0. 5 "pipe" (Some 1) (-75) 100
let fly_slow = Game.set_pwr_active fly_can_jump true 
               |> Game.set_powerup "slow" 100 100 
let fly_slow_inactive = Game.set_powerup "slow" 100 100 fly_can_jump 
let fly_slow_newpipe = Game.set_pwr_active pipe_offs true
                       |> Game.set_powerup "slow" 100 100

(*********************************Pipe 2 collisions****************************)
let fly_pipe_collision = Game.create (200., 150.) 10. 7 "pipe" (Some 2) 200 100
let i_fly_pipe_collision = Game.set_pwr_active fly_pipe_collision true 
                           |> Game.set_powerup "invincible" 0 0
let fly_pipe_nocollision = 
  Game.create (200., 150.) 10. 7 "pipe" (Some 2) 400 100
let i_fly_pipe_nocollision = Game.set_pwr_active fly_pipe_nocollision true 
                             |> Game.set_powerup "invincible" 0 0
let fly_pipe_collision_top = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 2) 200 100
let i_fly_pipe_collision_top = Game.set_pwr_active fly_pipe_collision_top true 
                               |> Game.set_powerup "invincible" 0 0
let fly_pipe_nocollision_top = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 2) 400 100
let i_fly_pipe_nocollision_top = 
  Game.set_pwr_active fly_pipe_nocollision true 
  |> Game.set_powerup "invincible" 0 0
let fly_pipe_no_collision_inbetween =
  Game.create (200., 300.) 10. 7 "pipe" (Some 2) 200 10
let i_fly_pipe_no_collision_inbetween = 
  Game.set_pwr_active fly_pipe_no_collision_inbetween true 
  |> Game.set_powerup "invincible" 0 0
let pipe2_inb_tophit = Game.create (200., 375.) 10. 7 "pipe" (Some 2) 200 100
let i_pipe2_inb_tophit = Game.set_pwr_active pipe2_inb_tophit true 
                         |> Game.set_powerup "invincible" 0 0 
let pipe2_inb_bothit = Game.create (200., 235.) 10. 7 "pipe" (Some 2) 200 100
let i_pipe2_inb_bothit = Game.set_pwr_active pipe2_inb_bothit true 
                         |> Game.set_powerup "invincible" 0 0

(*********************************Pipe 1 collisions****************************)
let fly_pipe_collision_1 = 
  Game.create (200., 150.) 10. 7 "pipe" (Some 1) 200 100
let i_fly_pipe_collision_1 = Game.set_pwr_active fly_pipe_collision_1 true 
                             |> Game.set_powerup "invincible" 0 0
let fly_pipe_collision_top_1 = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 1) 200 100
let i_fly_pipe_collision_top_1 = 
  Game.set_pwr_active fly_pipe_collision_top_1 true 
  |> Game.set_powerup "invincible" 0 0
let fly_pipe_nocollision_1 = 
  Game.create (200., 150.) 10. 7 "pipe" (Some 1) 400 100
let i_fly_pipe_nocollision_1 = Game.set_pwr_active fly_pipe_nocollision_1 true 
                               |> Game.set_powerup "invincible" 0 0
let fly_pipe_nocollision_top_1 = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 1) 400 100
let i_fly_pipe_nocollision_top_1 = 
  Game.set_pwr_active fly_pipe_nocollision_top_1 true 
  |> Game.set_powerup "invincible" 0 0
let fly_pipe_no_collision_inbetween_1 =
  Game.create (200., 300.) 10. 7 "pipe" (Some 1) 200 100
let i_fly_pipe_no_collision_inbetween_1 = 
  Game.set_pwr_active fly_pipe_no_collision_inbetween_1 true 
  |> Game.set_powerup "invincible" 0 0
let pipe1_inb_tophit = Game.create (200., 550.) 10. 7 "pipe" (Some 1) 200 100
let i_pipe1_inb_tophit = Game.set_pwr_active pipe1_inb_tophit true 
                         |> Game.set_powerup "invincible" 0 0
let pipe1_inb_bothit = Game.create (200., 434.) 10. 7 "pipe" (Some 1) 200 100
let i_pipe1_inb_bothit = Game.set_pwr_active pipe1_inb_tophit true 
                         |> Game.set_powerup "invincible" 0 0

(*********************************Pipe 0 collisions****************************)
let fly_pipe_collision_0 = 
  Game.create (200., 150.) 10. 7 "pipe" (Some 0) 200 100
let i_fly_pipe_collision_0 = Game.set_pwr_active fly_pipe_collision_0 true 
                             |> Game.set_powerup "invincible" 0 0
let fly_pipe_collision_top_0 = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 0) 200 100
let i_fly_pipe_collision_top_0 = 
  Game.set_pwr_active fly_pipe_collision_top_0 true 
  |> Game.set_powerup "invincible" 0 0  
let fly_pipe_nocollision_0 = 
  Game.create (200., 150.) 10. 7 "pipe" (Some 0) 400 100
let i_fly_pipe_nocollision_0 = Game.set_pwr_active fly_pipe_nocollision_0 true 
                               |> Game.set_powerup "invincible" 0 0
let fly_pipe_nocollision_top_0 = 
  Game.create (200., 590.) 10. 7 "pipe" (Some 0) 400 100
let i_fly_pipe_nocollision_top_0 = 
  Game.set_pwr_active fly_pipe_nocollision_top_0 true 
  |> Game.set_powerup "invincible" 0 0
let fly_pipe_no_collision_inbetween_0 =
  Game.create (200., 300.) 10. 7 "pipe" (Some 0) 200 100
let i_fly_pipe_no_collision_inbetween_0 = 
  Game.set_pwr_active fly_pipe_no_collision_inbetween_0 true 
  |> Game.set_powerup "invincible" 0 0
let pipe0_inb_tophit = Game.create (200., 475.) 10. 7 "pipe" (Some 0) 200 100
let i_pipe0_inb_tophit = Game.set_pwr_active pipe0_inb_tophit true 
                         |> Game.set_powerup "invincible" 0 0
let pipe0_inb_bothit = Game.create (200., 345.) 10. 7 "pipe" (Some 0) 200 100
let i_pipe0_inb_bothit = Game.set_pwr_active pipe0_inb_bothit true 
                         |> Game.set_powerup "invincible" 0 0


(********************************* Cactus Collisions ****************************)

let cactus_no_collision =
  Game.create (200., 150.) 10. 7 "cactus" (Some 0) 400 100

let run_can_jump = run_player_alive
let run_no_jump = Game.set_can_jump run_can_jump false
let run_above_ground = Game.create (200., 300.) 0. 5 "cactus" (Some 1) 400 100
let run_no_vel = Game.set_can_jump run_above_ground false
let cactus_offs = Game.create (200., 100.) 0. 5 "cactus" (Some 1) (-75) 100
let run_slow = Game.set_pwr_active run_can_jump true 
               |> Game.set_powerup "slow" 100 100
let run_slow_newcac = Game.set_pwr_active cactus_offs true 
                      |> Game.set_powerup "slow" 100 100
let run_collides = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 175 100

let pipe_test_player = Game.create (200.,100.) 0. 5 "pipe" (Some 0) (-75) 100
let cactus_test_player = Game.create (200.,100.) 0. 5 "pipe" (Some 1) (-75) 100

(*********************************Powerup Collisions***************************)
let fly_hitslow = Game.create (220., 360.) 10. 5 "pipe" (Some 1) 400 100 
                  |> Game.set_powerup "slow" 200 350
let fly_hitinvin = Game.create (220., 360.) 10. 5 "pipe" (Some 1) 400 100 
                   |> Game.set_powerup "invincible" 200 350

let run_hitslow = Game.create (200., 150.) 10. 5 "pipe" (Some 1) 400 100
                  |> Game.set_powerup "slow" 220 160
let run_hitinvin = Game.create (200., 150.) 10. 5 "pipe" (Some 1) 400 100
                   |> Game.set_powerup "invincible" 220 160

let game_tests = [
  jump_test "velocity after jumping in fly-state" fly_can_jump "go" (225.5);
  jump_test "downwards velocity after no jump in fly-state" 
    fly_no_jump "go" (5.5);
  jump_test "velocity after jumping in run-state" run_can_jump "run" (335.5);
  jump_test "downwards velocity in run-state" run_no_jump "run" (-4.5);
  jump_test "no jump during torun" fly_can_jump "torun" (5.5);
  gravity_test "fly-state, y dropping with 0 velocity" fly_no_vel "go" 399;
  gravity_test "fly-state, y increasing after jumping" fly_can_jump "go" 422;

  gravity_test "bomb-state, y dropping with 0 velocity" fly_no_vel "bomb" 399;
  gravity_test "bomb-state, y increasing after jumping" fly_can_jump "bomb" 422;

  gravity_test "run-state, y stays at 100" run_no_jump "run" 100;
  gravity_test "run-state, y dropping with 0 velocity" run_no_vel "run" 299;
  gravity_test 
    "run-state, y increasing after jumping" run_can_jump "run" 133;
  move_obs_test "fly-state, no powerups, pipe on screen" fly_can_jump "go" 395;
  move_obs_test "fly-state, no powerups, pipe off-screen" pipe_offs "go" 600;
  move_obs_test 
    "fly-state, time powerup active, pipe on screen" fly_slow "go" 399;
  move_obs_test "fly-state, time powerup active, pipe off screen" 
    fly_slow_newpipe "go" 600;
  move_obs_test 
    "run-state, no powerups, cactus on screen" run_can_jump "run" 419;
  move_obs_test 
    "run-state, no powerups, cactus off screen" cactus_offs "run" 624;
  move_obs_test "run-state, time powerup active, cactus on screen"
    run_slow "run" 423;
  move_obs_test "run-state, time powerup active, cactus off screen"
    run_slow_newcac "run" 624;
  pipe_type_change_test
    "run-state, no powerups, of type pipe" pipe_test_player "run" 0;
  pipe_type_change_test
    "run-state, no powerups, of type cactus" cactus_test_player "run" 1;
  pipe_type_change_test
    "go-state, no powerups, of type pipe" pipe_test_player "go" 0;
  pipe_type_change_test
    "go-state, no powerups, of type cactus" cactus_test_player "go" 1;
  pipe_type_change_test
    "togo-state, no powerups, of type pipe" pipe_test_player "togo" 0;
  pipe_type_change_test
    "togo-state, no powerups, of type cactus" cactus_test_player "togo" 1;
  pipe_type_change_test
    "torun-state, no powerups, of type pipe" pipe_test_player "torun" 0;
  pipe_type_change_test
    "torun-state, no powerups, of type cactus" cactus_test_player "torun" 1;
  pipe_type_change_test
    "tobomb-state, no powerups, of type pipe" pipe_test_player "tobomb" 0;
  pipe_type_change_test
    "tobomb-state, no powerups, of type cactus" cactus_test_player "tobomb" 1;
  pipe_type_change_test
    "todeath-state, no powerups, of type pipe" pipe_test_player "death" 0;
  pipe_type_change_test
    "todeath-state, no powerups, of type cactus" cactus_test_player "death" 1;
  pipe_type_change_test
    "tobomb-state, no powerups, of type pipe" pipe_test_player "bomb" 0;
  pipe_type_change_test
    "tobomb-state, no powerups, of type cactus" cactus_test_player "bomb" 1;

  collision_test "fly-state, player collides with pipe" 
    fly_pipe_collision "go" true;
  collision_test "fly-state, no collision with pipe"
    fly_can_jump "go" false;
  collision_test "run-state, player collides with cactus" 
    run_collides "run" true;
  collision_test "run-state, no collision with pipe" 
    run_can_jump "run" false;
  collision_test "fly state, player doesn't collide with pipe" 
    fly_pipe_nocollision "go" false; 
  collision_test "fly-state, player collides with top pipe" 
    fly_pipe_collision_top "go" true;
  collision_test "fly-state, player doesn't collides with top pipe" 
    fly_pipe_nocollision_top "go" false;
  collision_test "fly-state, player doesn't collides with top pipe" 
    fly_pipe_no_collision_inbetween "go" false;

  collision_test "fly-state, player collides with bottom pipe 1" 
    fly_pipe_collision_1 "go" true;
  collision_test "fly state, player doesn't collide with bottom pipe 1" 
    fly_pipe_nocollision_1 "go" false; 
  collision_test "fly-state, player collides with top pipe 1" 
    fly_pipe_collision_top_1 "go" true;
  collision_test "fly-state, player doesn't collides with top pipe 1" 
    fly_pipe_nocollision_top_1 "go" false;
  collision_test "fly-state, player does collides with inbetween pipe 1" 
    fly_pipe_no_collision_inbetween_1 "go" true;

  collision_test "fly-state, player collides with bottom pipe 0" 
    fly_pipe_collision_0 "go" true;
  collision_test "fly state, player doesn't collide with bottom pipe 0" 
    fly_pipe_nocollision_0 "go" false; 
  collision_test "fly-state, player collides with top pipe 0" 
    fly_pipe_collision_top_0 "go" true;
  collision_test "fly-state, player doesn't collides with top pipe 0" 
    fly_pipe_nocollision_top_0 "go" false;
  collision_test "fly-state, player does collides with inbetween pipe 0" 
    fly_pipe_no_collision_inbetween_0 "go" true;

  collision_test "fly-state, invin active, collides with side pipe set 2"
    i_fly_pipe_collision "go" false;
  collision_test "fly-state, invin active, collides with top pipe, set 2" 
    i_fly_pipe_collision_top "go" false;
  collision_test 
    "fly-state, invin active, doesn't collides with top pipe, set 2" 
    i_fly_pipe_nocollision "go" false;
  collision_test "fly-state, invin active, doesn't collide with top pipe, set 2" 
    i_fly_pipe_nocollision_top "go" false;
  collision_test 
    "fly-state, invin active, doesn't collide going in between, set 2" 
    i_fly_pipe_no_collision_inbetween "go" false;

  collision_test "fly-state, invin active, collides with bottom pipe, set 1" 
    i_fly_pipe_collision_1 "go" false;
  collision_test "fly-state, invin active, collides with top pipe, set 1" 
    i_fly_pipe_collision_top_1 "go" false;
  collision_test "fly-state, invin active, collides with top pipe, set 1" 
    i_fly_pipe_nocollision_1 "go" false;
  collision_test "fly-state, invin active, doesn't collide with top pipe, set 1" 
    i_fly_pipe_nocollision_top_1 "go" false;
  collision_test 
    "fly-state, invin active, doesn't collide going in between, set 1" 
    i_fly_pipe_no_collision_inbetween_1 "go" false;

  collision_test "fly-state, invin active, collides with bottom pipe, set 0" 
    i_fly_pipe_collision_0 "go" false;
  collision_test "fly-state, invin active, collides with top pipe, set 0" 
    i_fly_pipe_collision_top_0 "go" false;
  collision_test "fly-state, invin active, collides with top pipe, set 0" 
    i_fly_pipe_nocollision_0 "go" false;
  collision_test "fly-state, invin active, doesn't collide with top pipe, set 0" 
    i_fly_pipe_nocollision_top_0 "go" false;
  collision_test 
    "fly-state, invin active, doesn't collide going in between, set 0" 
    i_fly_pipe_no_collision_inbetween_0 "go" false;

  collision_test "fly-state, pipe set 2, gap collision w/ top pipe"
    pipe2_inb_tophit "go" true;
  collision_test "fly-state, pipe set 2, gap collision w/ bottom pipe"
    pipe2_inb_bothit "go" true;
  collision_test "fly-state, pipe set 2, invin active, no gap collision w/ top"
    i_pipe2_inb_tophit "go" false;
  collision_test "fly-state, pipe set 2, invin active, no gap collision w/ bot"
    i_pipe2_inb_bothit "go" false;
  collision_test "fly-state, pipe set 1, gap collision w/ top pipe"
    pipe1_inb_tophit "go" true;
  collision_test "fly-state, pipe set 1, gap collision w/ bottom pip"
    pipe1_inb_bothit "go" true;
  collision_test "fly-state, pipe set 1, invin active, no gap collision w/ top"
    i_pipe1_inb_tophit "go" false;
  collision_test "fly-state, pipe set 1, invin active, no gap collision w/ bot"
    i_pipe1_inb_tophit "go" false;
  collision_test "fly-state, pipe set 0, gap collsion w/ top pipe" 
    pipe0_inb_tophit "go" true;
  collision_test "fly-state, pipe set 0, gap collision w/ bottom pipe"
    pipe0_inb_bothit "go" true;
  collision_test "fly-state, pipe set 0, invin active, no gap collision w/ top"
    i_pipe0_inb_tophit "go" false;
  collision_test "fly-state, pipe set 0, invin active, no gap collision w/ bot" 
    i_pipe0_inb_bothit "go" false;

  collision_test "run state, cactus, not colliding" 
    cactus_no_collision "run" false;

  powerup_collision_test "fly-state, collision with slow powerup" 
    fly_hitslow "go" true;
  powerup_collision_test "fly-state, collision with invin powerup"
    fly_hitinvin "go" true;
  powerup_collision_test "run-state, collision with slow powerup"
    run_hitslow "run" true;
  powerup_collision_test "run-state, collision with slow powerup"
    run_hitslow "run" true;


  move_powerup_test "fly-state, time powerup inactive" 
    fly_slow_inactive "go" (95, 100);
  move_powerup_test "fly-state, time powerup active" 
    fly_slow "go" (0, 0);

  move_powerup_test "run-state, time powerup inactive" 
    fly_slow_inactive "run" (95, 100);
  move_powerup_test "run-state, time powerup active" 
    fly_slow "run" (0, 0);

]
(******************************************************************************)
let suite =
  "test lists"  >::: List.flatten [ 
    state_tests;
    game_tests
  ]

let _ = run_test_tt_main suite








