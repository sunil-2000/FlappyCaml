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
let go_player_pipe = Game.create (200., 99.) 10. 7 "pipe" (Some 2) 50 100

let run_player_alive = Game.create (200., 100.) 10. 7 "cactus" (Some 1) 400 100

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
  check_test "fly-state, player hits obstacle" death_state go_player_pipe "death";
]

(******************************************************************************)

let suite =
  "test lists"  >::: List.flatten [ 
    state_tests
  ]

let _ = run_test_tt_main suite

