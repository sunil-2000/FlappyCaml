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
let go_player_transition = Game.create (200., 400.) 10. 4 "pipe" (Some 2) 400 100
                           |> Game.set_score 4

let run_player_alive = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 400 100
let run_player_dead = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 175 100
                      |> Game.set_collision true
let run_player_transition = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 400 100
                            |> Game.set_score 4

let togo_player_yes = Game.create (200., 400.) 10. 7 "pipe" (Some 2) 400 100
let togo_player_no = Game.create (200., 349.) 10. 7 "pipe" (Some 2) 400 100

let torun_player_yes = Game.create (200., 97.) 10. 7 "cactus" (Some 1) 400 100
let torun_player_no = Game.create (200., 101.) 10. 7 "cactus" (Some 1) 400 100

let dead_player_gameover = Game.create (200., -50.) 10. 7 "cactus" (Some 1) 400 100 
let dead_player_nogameover = Game.create (200., -49.) 10. 7 "cactus" (Some 1) 400 100

let check_test 
    (name: string)
    (state: State.t)
    (player: Game.t)
    (expected_output: string) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (State.check state player |> State.string_of_state)

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
  Game.update 1. player state_string

let update_test
    (name: string)
    (player: Game.t)
    (state_string: string)
    (getter)
    (expected_output) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (Game.update 1. player state_string |> getter) 

let jump_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: float) : test = 
  update_test name player state_string Game.get_velocity expected_output

let gravity_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: float) : test = 
  update_test name player state_string Game.get_y expected_output 

let move_obs_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string Game.get_obs_x expected_output

let collision_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string Game.get_collision expected_output

let move_powerup_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int * int) : test = 
  update_test name player state_string Game.get_pwr_pos expected_output

let score_update_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: int) : test = 
  update_test name player state_string Game.get_score expected_output

let score_updated_test 
    (name: string)
    (player: Game.t)
    (state_string: string)
    (expected_output: bool) : test = 
  update_test name player state_string Game.get_score_updated expected_output 

let fly_can_jump = go_player_alive
let fly_no_jump = Game.set_can_jump fly_can_jump false

let game_tests = [
  jump_test "velocity after jumping in fly-state" fly_can_jump "go" 10.;
]
(******************************************************************************)
let suite =
  "test lists"  >::: List.flatten [ 
    state_tests;
    game_tests
  ]

let _ = run_test_tt_main suite

