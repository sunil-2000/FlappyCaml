open State 
open Graphics 
open Game 
open OUnit2

(***************************State Module Tests*********************************)
let start_state = State.make_state () 
let go_state = State.make_state () |> State.set_go 
let run_state = State.make_state () |> State.set_run 
let togo_state = State.make_state () |> State.set_togo 
let torun_state = State.make_state () |> State.set_torun 
let death_state = State.make_state () |> State.set_death

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
  check_test "fly-state, alive player" go_state go_player_alive "go";
  check_test "fly-state, player hits floor" death_state go_player_floor "death";
  check_test "fly-state, player hits obstacle" 
    death_state go_player_pipe "death";
  check_test "fly-state, player transitions to ToRun" 
    torun_state go_player_transition "torun";
  check_test "run-state, alive player" run_state run_player_alive "run";
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

let collision_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string 0.1 Game.get_collision expected_output

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

let fly_can_jump = go_player_alive
let fly_no_jump = Game.set_can_jump fly_can_jump false
let no_vel = Game.create (200., 400.) 0. 5 "pipe" (Some 1) 400 100 
let fly_no_vel = Game.set_can_jump no_vel false
let pipe_offs = Game.create (200., 400.) 0. 5 "pipe" (Some 1) (-75) 100

let run_can_jump = run_player_alive
let run_no_jump = Game.set_can_jump run_can_jump false
let run_above_ground = Game.create (200., 300.) 0. 5 "cactus" (Some 1) 400 100
let run_no_vel = Game.set_can_jump run_above_ground false
let cactus_offs = Game.create (200., 100.) 0. 5 "cactus" (Some 1) (-75) 100

let game_tests = [
  jump_test "velocity after jumping in fly-state" fly_can_jump "go" (225.5);
  jump_test "downwards velocity after no jump in fly-state" 
    fly_no_jump "go" (5.5);
  jump_test "velocity after jumping in run-state" run_can_jump "run" (335.5);
  jump_test "downwards velocity in run-state" run_no_jump "run" (-4.5);
  jump_test "no jump during torun" fly_can_jump "torun" (5.5);
  gravity_test "fly-state, y dropping with 0 velocity" fly_no_vel "go" 399;
  gravity_test "fly-state, y increasing after jumping" fly_can_jump "go" 422;
  gravity_test "run-state, y stays at 100" run_no_jump "run" 100;
  gravity_test "run-state, y dropping with 0 velocity" run_no_vel "run" 299;
  gravity_test 
    "run-state, y increasing after jumping" run_can_jump "run" 133;
  move_obs_test "fly-state, no powerups, pipe on screen" fly_can_jump "go" 395;
  move_obs_test "fly-state, no powerups, pipe off-screen" pipe_offs "go" 600;
  move_obs_test 
    "run-state, no powerups, cactus on screen" run_can_jump "run" 395;
  move_obs_test 
    "run-state, no powerups, cactus off screen" cactus_offs "run" 600;
]
(******************************************************************************)
let suite =
  "test lists"  >::: List.flatten [ 
    state_tests;
    game_tests
  ]

let _ = run_test_tt_main suite

