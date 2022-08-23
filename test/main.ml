open OUnit2
open Gametest
open Statetest
open Solvertest
open Entropytest

(* TEST PLAN: The Game, State, Solver, and Entropy modules were tested
   primarily via OUnit testing. The main Wordle module, which asks the
   user to play Wordle, was tested many times manually. Additionally,
   the modules under the gamemode folder, which allows the player the
   option to play against the AI at different difficulties, was tested
   manually by numerous users.

   The Game module was tested via black-box testing, in that we tested
   the function purely based on function specification and by looking at
   the data files to confirm that words were processed correctly. It was
   also tested via randomized testing to confirm that every word
   processed fell under the property that it was 5 letters long. The
   State module was tested via glass-box testing, as we had to know the
   format of how certain lists were outputted. We tested cases such as
   multiple words being guessed, double letter words, and double letter
   guesses. The Solver module was tested primarily with black-box
   testing for all methods other than those pertaining to the frequency
   algorithm. We had to know how the frequency score was calculated to
   confirm its validity while testing. Entopy was tested using both
   black-box and glass-box testing, as we had to see the format of how
   abstract permutations were created.

   This testing approach is valid since certain aspects such as the user
   playing the game can only be tested manually, not with OUnit tests.
   Manually testing these aspects also allow for a more unexpected cases
   to be covered than if a few OUnit tests were to be conducted.
   Black-box testing is sufficient for most methods as we are only
   testing their main functionality, and many functions do not have many
   different paths to keep track of.*)

let suite =
  "test suite for 3110dle"
  >::: List.flatten
         [ game_tests; state_tests; solver_tests; entropy_tests ]

let _ = run_test_tt_main suite