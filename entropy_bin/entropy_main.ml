open Wordle
open Game
open State
open Solver
open Entropy

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"
let permutations = generate_perms 0

let main () =
  let gme = from_txt filename_g filename_w in
  first_entropies (get_guesslist gme) (get_guesslist gme) permutations

let () = main ()
