open OUnit2
open Gametest
open Statetest
open Solvertest
open Entropytest

let suite =
  "test suite"
  >::: List.flatten
         [ game_tests; state_tests; solver_tests; entropy_tests ]

let _ = run_test_tt_main suite