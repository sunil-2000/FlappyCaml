open State 
open Graphics 
open Game 
open Main
open OUnit2

(***************************State Module Tests*********************************)
let go_player_alive = Game.create (50., 400.) 10. 10 "pipe" (Some 2) 400 100
let go_player_floor = Game.create (50., 99.) 10. 10 "pipe" (Some 2) 400 100
let go_player_pipe = Game.create (50., 99.) 10. 10 "pipe" (Some 2) 400 100

let check_test 
    (name: string)
    (state: State.t)
    (player: Game.t)
    (expected_output: State.t) : test = 
  name >:: fun _ -> 
    assert_equal expected_output (State.check state player)




(******************************************************************************)

let suite =
  "test lists"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite

