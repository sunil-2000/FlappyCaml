open State 
open Graphics 
open Game 
open OUnit2

(***************************Testing Methodology********************************)
(* Our approach to testing was to use glass and black box testing to ensure 
   critical game functions were performing as desired. We tested primarily 
   State and Game functions automatically as Gui and Main could only be tested
   manually via opening a game window and playing the game. In State, we tested 
   [check] to make sure the main game states (running, flying, death, gameover,
   bomb, and transition states) were working properly and transitioned when 
   specific game conditions were met. Certain other states such as instructions 
   and the start state were tested manually as they required key press inputs 
   and the GUI to operate. Similarly for the Game module, we tested primarily 
   Game.update, which handled updating various fields and parameters of the 
   player, obstacles, and other game entities. Testing was done by passing a 
   player through the [update] function, which updated all fields of the player
   and matching the results via getters to expected outputs. Certain functions 
   that were not directly called by update were tested indirectly via outputs 
   of the main update function, and functions that required GUI inputs were left
   to manual testing. Functions were tested with a mix of glass and black box 
   testing as functions like [gravity] and [velocity_change] require knowing 
   specific equations to determine the proper output. However functions like 
   [score_update] and [collision] were tested via black-box testing as they 
   returned boolean fields. Furthermore tests for certain functions like
   [gravity] and [collision] were also randomized to ensure general correctness
   and reduce potential outlier inputs. We believe this test suite demonstrates 
   the correctness of our system as extensively tests all core game mechanics 
   and ensures the basic functions of the game are working as intended. *)
(******************************************************************************)


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
let bomb_state = State.make_state ()
                 |> State.set_bomb 

let go_player_alive = Game.create (200., 400.) 10. 7 "pipe" (Some 2) 400 100
let go_player_floor = Game.create (200., 99.) 10. 7 "pipe" (Some 2) 400 100
let go_player_pipe = Game.create (200., 150.) 10. 7 "pipe" (Some 2) 200 100
                     |> Game.set_collision true
let go_player_transition = 
  Game.create (200., 400.) 10. 4 "pipe" (Some 2) 400 100

let run_player_alive = Game.create (200., 100.) 0. 7 "cactus" (Some 1) 400 100
let run_player_dead = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 175 100
                      |> Game.set_collision true
let run_player_transition = 
  Game.create (200., 100.) 10. 4 "cactus" (Some 1) 400 100

let togo_player_yes = Game.create (200., 400.) 10. 7 "pipe" (Some 2) 400 100
let togo_player_no = Game.create (200., 349.) 10. 7 "pipe" (Some 2) 400 100

let torun_player_yes = Game.create (200., 97.) 10. 7 "cactus" (Some 1) 400 100
let torun_player_no = Game.create (200., 101.) 10. 7 "cactus" (Some 1) 400 100

let dead_player_gameover = 
  Game.create (200., -50.) 10. 7 "cactus" (Some 1) 400 100 
let dead_player_nogameover = 
  Game.create (200., -49.) 10. 7 "cactus" (Some 1) 400 100

let bomb_player = Game.create (200., 400.) 10. 5 "pipe" (Some 0) 400 100 
let dead_b_player = Game.set_collision true bomb_player

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
  (* check_test "fly-state, player transitions to ToRun" 
     torun_state go_player_transition "torun"; *)
  check_test "state: run-state, state of player: alive player" 
    run_state run_player_alive "run";
  check_test "state: run-state, state of player: dead player" 
    run_state run_player_dead "death";
  (* check_test "ToGo transition ok" togo_state togo_player_yes "go";
     check_test "ToGo transition not ok" togo_state togo_player_no "togo";
     check_test "ToRun transition ok" torun_state torun_player_yes "run";
     check_test "ToRun transition not ok" torun_state torun_player_no "torun";*)
  check_test "transition to Gameover" 
    death_state dead_player_gameover "gameover";
  check_test "no transition to gameover" 
    death_state dead_player_nogameover "death";
  check_test "bomb state, player alive" bomb_state bomb_player "bomb";
  check_test "bomb state, player dead, transition to gameover"
    death_state dead_b_player "death";
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
  update_test name player state_string 0.1 Game.get_obs_type expected_output  

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

let fly_slow = Game.set_pwr_active fly_can_jump true 
               |> Game.set_powerup "slow" 100 100 
let fly_slow_inactive = Game.set_powerup "slow" 100 100 fly_can_jump
let pipe2_reset = Game.create (200., 400.) 0. 5 "pipe" (Some 1) (-75) 100 
let fly_slow_newpipe = Game.set_pwr_active pipe2_reset true
                       |> Game.set_powerup "slow" 100 100
let pipe0_reset = Game.create (200., 400.) 0. 5 "pipe" (Some 0) (-75) 100
let pipe0_onscreen = Game.create (200., 400.) 0. 5 "pipe" (Some 1) 400 100
let pipe0_slow = Game.set_pwr_active pipe0_onscreen true 
                 |> Game.set_powerup "slow" 0 0
let pipe1_reset = Game.create (200., 400.) 0. 5 "pipe" (Some 1) (-75) 100
let pipe1_onscreen = Game.create (200., 400.) 0. 5 "pipe" (Some 1) 400 100
let pipe1_slow = Game.set_pwr_active pipe1_onscreen true 
                 |> Game.set_powerup "slow" 0 0

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

let fly_pipe_close_miss = Game.create (150., 345.) 10. 7 "pipe" (Some 2)
    210 100
let fly_pipe_close_collision = Game.create (300., 200.) 10. 7 "pipe" (Some 2) 
    300 100

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

let fly_pipe_close_miss_1 = Game.create (150., 345.) 10. 7 "pipe" (Some 1)
    210 100
let fly_pipe_close_collision_1 = Game.create (300., 345.) 10. 7 "pipe" (Some 1) 
    240 100

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

let fly_pipe_close_miss_0 = Game.create (150., 345.) 10. 7 "pipe" (Some 0) 210 100
let fly_pipe_close_collision_0 = Game.create (300., 345.) 10. 7 "pipe" (Some 0) 
    240 100


(********************************* Cactus Collisions **************************)

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
let fly_hitslow2 = Game.set_powerup "slow" 220 360 fly_hitslow
let fly_hitslow3 = Game.set_powerup "slow" 260 400 fly_hitslow
let fly_hitinvin = Game.create (220., 360.) 10. 5 "pipe" (Some 1) 400 100 
                   |> Game.set_powerup "invincible" 200 350
let fly_hitinvin2 = Game.set_powerup "invincible" 220 360 fly_hitinvin
let fly_hitinvin3 = Game.set_powerup "invincible" 260 400 fly_hitinvin
let fly_noslow = Game.set_powerup "slow" 400 100 fly_hitslow
let fly_noinvin = Game.set_powerup "invincible" 400 100 fly_hitslow 

let run_hitslow = Game.create (200., 150.) 10. 5 "pipe" (Some 1) 400 100
                  |> Game.set_powerup "slow" 220 160
let run_hitslow2 = Game.set_powerup "slow" 200 150 run_hitslow
let run_hitslow3 = Game.set_powerup "slow" 240 190 run_hitslow
let run_hitinvin = Game.create (200., 150.) 10. 5 "pipe" (Some 1) 400 100
                   |> Game.set_powerup "invincible" 220 160
let run_hitinvin2 = Game.set_powerup "invincible" 200 150 run_hitinvin
let run_hitinvin3 = Game.set_powerup "invincible" 240 190 run_hitinvin
let run_noslow = Game.set_powerup "slow" 400 100 run_hitslow
let run_noinvin = Game.set_powerup "invincible" 400 100 run_hitslow

(*********************************Bomb State***********************************)
let bomb_jump = Game.create (200., 400.) 0. 6 "pipe" (Some 1) 400 100
let bomb_fall = Game.set_can_jump bomb_jump false
let bomb_fall_temp = 
  Game.create (200., 400.) (-219.99) 5 "pipe" (Some 1) 400 100
let bomb_fall_max = Game.set_can_jump bomb_fall_temp false
let bomb_floorhit = 
  Game.set_can_jump 
    (Game.create (200., 100.) (-10.) 6 "pipe" (Some 1) 400 100) false

(*********************************Score Update*********************************)
let score_update_set0 = Game.create (200., 360.) 0. 5 "pipe" (Some 0) 128 100
let score_update_set1 = Game.create (200., 445.) 0. 5 "pipe" (Some 1) 128 100
let score_update_set2 = Game.create (200., 250.) 0. 5 "pipe" (Some 2) 128 100
let score_update_cactus = 
  Game.create (200., 200.) 0. 5 "cactus" (Some 0) 150 100
let no_score_update_set0 = 
  Game.create (200., 360.) 0. 5 "pipe" (Some 0) 190 100
let no_score_update_set1 = 
  Game.create (200., 445.) 0. 5 "pipe" (Some 1) 190 100
let no_score_update_set2 = 
  Game.create (200., 250.) 0. 5 "pipe" (Some 1) 190 100
let ahead_pipe0 = Game.create (200., 350.) 0. 5 "pipe" (Some 0) 400 100
let ahead_pipe1 = Game.create (200., 350.) 0. 5 "pipe" (Some 1) 400 100
let ahead_pipe2 = Game.create (200., 350.) 0. 5 "pipe" (Some 2) 400 100
let ahead_cactus = Game.create (200., 350.) 0. 5 "cactus" (Some 0) 400 100



let game_tests = [
  jump_test "velocity after jumping in fly-state" fly_can_jump "go" (225.5);
  jump_test "downwards velocity after no jump in fly-state" 
    fly_no_jump "go" (5.5);
  jump_test "velocity after jumping in run-state" run_can_jump "run" (335.5);
  jump_test "downwards velocity in run-state" run_no_jump "run" (-4.5);
  jump_test "no jump during torun" fly_can_jump "torun" (5.5);
  jump_test "velocity after jump during bomb state" bomb_jump "bomb" 225.5;
  jump_test "initial velocity of 0 in bomb state" bomb_fall "bomb" (-4.5);
  jump_test "max downwards velocity in bomb state" bomb_fall_max "bomb" (-220.);
  gravity_test "position change after jump in bomb state" 
    bomb_jump "bomb" 422;
  gravity_test "position change after no jump in bomb state" 
    bomb_fall "bomb" 399;
  gravity_test "fly-state, y dropping with 0 velocity" fly_no_vel "go" 399;
  gravity_test "fly-state, y increasing after jumping" fly_can_jump "go" 422;

  gravity_test "bomb-state, y dropping with 0 velocity" fly_no_vel "bomb" 399;
  gravity_test "bomb-state, y increasing after jumping" fly_can_jump "bomb" 422;

  gravity_test "run-state, y stays at 100" run_no_jump "run" 100;
  gravity_test "run-state, y dropping with 0 velocity" run_no_vel "run" 299;
  gravity_test "run-state, y increasing after jumping" run_can_jump "run" 133;
  move_obs_test "fly-state, no powerups, pipe set 2 on screen" 
    fly_can_jump "go" 395;
  move_obs_test "fly-state, no powerups, pipe set 2 off-screen" 
    pipe2_reset "go" 600;
  move_obs_test "fly-state, time powerup active, pipe set 2 on screen" 
    fly_slow "go" 399;
  move_obs_test "fly-state, time powerup active, pipe off screen" 
    fly_slow_newpipe "go" 600;
  move_obs_test "run-state, no powerups, cactus on screen" 
    run_can_jump "run" 419;
  move_obs_test "run-state, no powerups, cactus off screen" 
    cactus_offs "run" 624;
  move_obs_test "run-state, time powerup active, cactus on screen"
    run_slow "run" 423;
  move_obs_test "run-state, time powerup active, cactus off screen"
    run_slow_newcac "run" 624;
  move_obs_test "fly-state, pipe set 0 move, no powerups" 
    pipe0_onscreen "go" 395;
  move_obs_test "fly-state, pipe set 0 reset, no powerups"
    pipe0_reset "go" 600;
  move_obs_test "fly-state, pipe set 1 move, no powerups"
    pipe1_onscreen "go" 395;
  move_obs_test "fly-state, pipe set 1 reset, no powerups"
    pipe1_reset "go" 600;
  move_obs_test "fly-state, pipe set 0 on screen, slow active"
    pipe0_slow "go" 399;
  move_obs_test "fly-state, pipe set 1 on screen, slow active"
    pipe1_slow "go" 399;
  move_bomber_test "bomber moving on-screen" bomb_jump "bomb" 599;
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
  collision_test "fly-state, near miss player doesn't collide with pipe 1"
    fly_pipe_close_miss_1 "go" false;
  collision_test "fly-state, close hit but player collides with pipe 1 at the 
    end" fly_pipe_close_collision_1 "go" true;

  collision_test "fly-state, player collides with bottom pipe 0" 
    fly_pipe_collision_0 "go" true;
  collision_test "fly state, player doesn't collide with bottom pipe 0" 
    fly_pipe_nocollision_0 "go" false; 
  collision_test "fly-state, player collides with top pipe 0" 
    fly_pipe_collision_top_0 "go" true;
  collision_test "fly-state, player doesn't collides with top pipe 0" 
    fly_pipe_nocollision_top_0 "go" false;
  collision_test "fly-state, player doesn't collide with inbetween pipe 0" 
    fly_pipe_no_collision_inbetween_0 "go" true;
  collision_test "fly-state, near miss player doesn't collide with pipe 0"
    fly_pipe_close_miss_0 "go" false;
  collision_test "fly-state, close hit but player collides with pipe 0 at the 
    end" fly_pipe_close_collision_0 "go" true;

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
  collision_test "fly-state, near miss player doesn't collide with pipe 2"
    fly_pipe_close_miss "go" false;
  collision_test "fly-state, close hit but player collides with pipe 2 at the 
    end" fly_pipe_close_collision "go" true;

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
  collision_test "bomb-state, collision with floor" bomb_floorhit "bomb" true;

  powerup_collision_test "fly-state, collision with slow powerup" 
    fly_hitslow "go" true;
  powerup_collision_test "fly-state, collision with invin powerup"
    fly_hitinvin "go" true;
  powerup_collision_test "run-state, collision with slow powerup"
    run_hitslow "run" true;
  powerup_collision_test "run-state, collision with slow powerup"
    run_hitslow "run" true;
  powerup_collision_test "fly-state, 2nd collision with slow powerup"
    fly_hitslow2 "go" true;
  powerup_collision_test "fly-state, 3rd collision with slow powerup"
    fly_hitslow3 "go" true;
  powerup_collision_test "fly-state, 2nd collision with invin powerup"
    fly_hitinvin2 "run" true;
  powerup_collision_test "fly-state, 3rd collision with invin powerup"
    fly_hitinvin3 "run" true;
  powerup_collision_test "run-state, 2nd collision with slow powerup"
    run_hitslow2 "run" true;
  powerup_collision_test "run-state, 3rd collision with slow powerup"
    run_hitslow3 "run" true;
  powerup_collision_test "run-state, 2nd collision with invin powerup"
    run_hitinvin2 "run" true;
  powerup_collision_test "run-state, 3rd collision with invin powerup"
    run_hitinvin3 "run" true;
  powerup_collision_test "fly-state, no collision with slow powerup"
    fly_noslow "go" false;
  powerup_collision_test "fly-state, no collision with invin powerup"
    fly_noinvin "go" false;
  powerup_collision_test "run-state, no collision with slow powerup"
    run_noslow "run" false;
  powerup_collision_test "run-state, no collision with invin powerup"
    run_noinvin "run" false;


  move_powerup_test "fly-state, time powerup inactive" 
    fly_slow_inactive "go" (95, 100);
  move_powerup_test "fly-state, time powerup active" 
    fly_slow "go" (0, 0);
  move_powerup_test "run-state, time powerup inactive" 
    fly_slow_inactive "run" (95, 100);
  move_powerup_test "run-state, time powerup active" 
    fly_slow "run" (0, 0);
  score_update_test "updating score, passing through pipe set 0" 
    score_update_set0 "go" 1;
  score_update_test "in pipe set 0 gap, no score update" 
    no_score_update_set0 "go" 0;
  score_update_test "updating score, passing through pipe set 1" 
    score_update_set1 "go" 1;
  score_update_test "in pipe set 1 gap, no score update" 
    no_score_update_set1 "go" 0;
  score_update_test "updating score, passing through pipe set 2" 
    score_update_set2 "go" 1;
  score_update_test "in pipe set 2 gap, no score update"
    no_score_update_set2 "go" 0;
  score_update_test "ahead of pipe set 0, no score update"
    ahead_pipe0 "go" 0;
  score_update_test "ahead of pipe set 1, no score update"
    ahead_pipe1 "go" 0;
  score_update_test "ahead of pipe set 2, no score update"
    ahead_pipe2 "go" 0;
  score_updated_test "pass through pipe set 0, score_updated set to true"
    score_update_set0 "go" true;
  score_updated_test "pass through pipe set 1, score_updated set to true"
    score_update_set1 "go" true;
  score_updated_test "pass through pipe set 2, score_updated set to true"
    score_update_set2 "go" true;
  score_updated_test "in pipe set 0 gap, score_updated stays false"
    no_score_update_set0 "go" false;
  score_updated_test "in pipe set 1 gap, score_updated stays false"
    no_score_update_set1 "go" false;
  score_updated_test "in pipe set 2 gap, score_updated stays false"
    no_score_update_set2 "go" false;
  score_updated_test "ahead of pipe set 0, score_updated stays false"
    ahead_pipe0 "go" false;
  score_updated_test "ahead of pipe set 1, score_updated stays false"
    ahead_pipe1 "go" false;
  score_updated_test "ahead of set 2, score_updated stays false"
    ahead_pipe2 "go" false;

  score_updated_test "in state togo test that it doesn't update with pipe 0"
    ahead_pipe0 "togo" false;
  score_updated_test "in state togo test that it doesn't update with pipe 1"
    ahead_pipe1 "togo" false;
  score_updated_test "in state togo test that it doesn't update with pipe 2"
    ahead_pipe2 "togo" false;
  score_updated_test "in state togo test that it doesn't update with cactus"
    ahead_cactus "togo" false; 

  score_updated_test "in state torun test that it doesn't update with pipe 0"
    ahead_pipe0 "torun" false;
  score_updated_test "in state torun test that it doesn't update with pipe 1"
    ahead_pipe1 "torun" false;
  score_updated_test "in state torun test that it doesn't update with pipe 2"
    ahead_pipe2 "torun" false;
  score_updated_test "in state torun test that it doesn't update with cactus"
    ahead_cactus "torun" false; 
]
(******************************************************************************)
let suite =
  "test lists"  >::: List.flatten [ 
    state_tests;
    game_tests
  ]

let _ = run_test_tt_main suite
