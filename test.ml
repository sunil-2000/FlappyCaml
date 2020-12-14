open State 
open Graphics 
open Game 
open Main
open OUnit2



let suite =
  "test lists"  >::: List.flatten [
  ]

let _ = run_test_tt_main suite

