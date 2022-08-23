open OUnit2
open Wordle
open Game
open State

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"
let gme = from_txt filename_g filename_w
let sort_uniq lst = List.sort_uniq compare lst

(********************************************************************
   Testing State
 ********************************************************************)

(* helper functions *)
let last_guess_test
    (name : string)
    (state : State.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (last_guess state)

let prev_guesses_test
    (name : string)
    (state : State.t)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (previous_guesses state)

let absent_letters_test
    (name : string)
    (state : State.t)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal
    (sort_uniq expected_output)
    (state |> absent_letters |> sort_uniq)

let rec sort_indices lst =
  match lst with
  | [] -> []
  | (c, ilist) :: t -> (c, sort_uniq ilist) :: sort_indices t

let sort_ci_listlist lst = lst |> sort_indices |> sort_uniq

let misp_letters_test
    (name : string)
    (state : State.t)
    (expected_output : (char * int list) list) : test =
  name >:: fun _ ->
  assert_equal
    (sort_ci_listlist expected_output)
    (state |> misplaced_letters |> sort_ci_listlist)

let cor_letters_test
    (name : string)
    (state : State.t)
    (expected_output : (char * int list) list) : test =
  name >:: fun _ ->
  assert_equal
    (sort_ci_listlist expected_output)
    (state |> correct_letters |> sort_ci_listlist)

let get_cor_test
    (name : string)
    (state : State.t)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal
    (sort_uniq expected_output)
    (state |> get_cor |> sort_uniq)

let get_misp_test
    (name : string)
    (state : State.t)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal
    (sort_uniq expected_output)
    (state |> get_misp |> sort_uniq)

let num_guess_test
    (name : string)
    (state : State.t)
    (expected_output : int) : test =
  name >:: fun _ -> assert_equal expected_output (num_guess state)

let extract_result = function
  | Legal t -> t
  | Illegal -> failwith "illegal guess"

let illegal_guess_test
    (name : string)
    (guess : string)
    (game : Game.t)
    (state : State.t) =
  name >:: fun _ ->
  assert_raises (Failure "illegal guess") (fun () ->
      move guess game state |> extract_result)

(* initialize test game with final word set as camel*)
let camel_gme = "camel" |> set_final_word gme
let camel_init = init_state camel_gme

let illegal_guess_tests =
  [
    illegal_guess_test "aaaaa is an illegal guess" "aaaaa" camel_gme
      camel_init;
    illegal_guess_test "hi is an illegal guess" "hi" camel_gme
      camel_init;
    illegal_guess_test "longword is an illegal guess" "longword"
      camel_gme camel_init;
    illegal_guess_test "_____ is an illegal guess" "_____" camel_gme
      camel_init;
    illegal_guess_test "zyxwv is an illegal guess" "zyxwv" camel_gme
      camel_init;
  ]

let camel_init_tests =
  [
    last_guess_test "last guess of init is '' " camel_init "";
    prev_guesses_test "previous guesses of init is []" camel_init [];
    absent_letters_test "absent letters of init is []" camel_init [];
    misp_letters_test "misp letters of init is []" camel_init [];
    cor_letters_test "cor letters of init is []" camel_init [];
    get_cor_test "cor keys of init is []" camel_init [];
    get_misp_test "misp keys of init is []" camel_init [];
    num_guess_test "num guesses of init is 0" camel_init 0;
  ]

(* guess word with all absent letters *)
let camel_proud_1 = move "proud" camel_gme camel_init |> extract_result

let camel_proud_1_tests =
  [
    last_guess_test "last guess of camel_proud_1 is proud" camel_proud_1
      "proud";
    prev_guesses_test "previous guesses of camel_proud_1 is [proud]"
      camel_proud_1 [ "proud" ];
    absent_letters_test
      "absent letters of camel_proud_1 is [d, o, p, r, u]" camel_proud_1
      [ 'd'; 'o'; 'p'; 'r'; 'u' ];
    misp_letters_test "misp letters of camel_proud_1 is []"
      camel_proud_1 [];
    cor_letters_test "cor letters of camel_proud_1 is []" camel_proud_1
      [];
    get_cor_test "cor keys of camel_proud_1 is []" camel_proud_1 [];
    get_misp_test "misp keys of camel_proud_1 is []" camel_proud_1 [];
    num_guess_test "num guesses of camel_proud_1 is 1" camel_proud_1 1;
  ]

(* guess word with misplaced letters *)
let camel_smile_1 = move "smile" camel_gme camel_init |> extract_result

let camel_smile_1_tests =
  [
    last_guess_test "last guess of camel_smile_1 is smile" camel_smile_1
      "smile";
    prev_guesses_test "previous guesses of camel_smile_1 is [smile]"
      camel_smile_1 [ "smile" ];
    absent_letters_test "absent letters of camel_smile_1 is [i, s]"
      camel_smile_1 [ 'i'; 's' ];
    misp_letters_test "misp letters of camel_smile_1" camel_smile_1
      [ ('e', [ 4 ]); ('l', [ 3 ]); ('m', [ 1 ]) ];
    cor_letters_test "cor letters of camel_smile_1 is []" camel_smile_1
      [];
    get_cor_test "cor keys of camel_smile_1 is []" camel_smile_1 [];
    get_misp_test "misp keys of camel_smile_1 is [e, l, m]"
      camel_smile_1 [ 'e'; 'l'; 'm' ];
    num_guess_test "num guesses of camel_smile_1 is 1" camel_smile_1 1;
  ]

(* guess word with misplaced and correct letters *)
let camel_calls_1 = move "calls" camel_gme camel_init |> extract_result

let camel_calls_1_tests =
  [
    last_guess_test "last guess of camel_calls_1 is calls" camel_calls_1
      "calls";
    prev_guesses_test "previous guesses of camel_calls_1 is [calls]"
      camel_calls_1 [ "calls" ];
    absent_letters_test "absent letters of camel_calls_1 is [s]"
      camel_calls_1 [ 's' ];
    misp_letters_test "misp letters of camel_calls_1" camel_calls_1
      [ ('l', [ 2; 3 ]) ];
    cor_letters_test "cor letters of camel_calls_1" camel_calls_1
      [ ('a', [ 1 ]); ('c', [ 0 ]) ];
    get_cor_test "cor keys of camel_calls_1 is []" camel_calls_1
      [ 'a'; 'c' ];
    get_misp_test "misp keys of camel_calls_1 is [l]" camel_calls_1
      [ 'l' ];
    num_guess_test "num guesses of camel_calls_1 is 1" camel_calls_1 1;
  ]

(* guess correct word *)
let camel_camel_1 = move "camel" camel_gme camel_init |> extract_result

let camel_camel_1_tests =
  [
    last_guess_test "last guess of camel_camel_1 is camel" camel_camel_1
      "camel";
    prev_guesses_test "previous guesses of camel_camel_1 is [camel]"
      camel_camel_1 [ "camel" ];
    absent_letters_test "absent letters of camel_camel_1 is []"
      camel_camel_1 [];
    misp_letters_test "misp letters of camel_camel_1 is []"
      camel_camel_1 [];
    cor_letters_test "cor letters of camel_camel_1" camel_camel_1
      [
        ('a', [ 1 ]);
        ('c', [ 0 ]);
        ('e', [ 3 ]);
        ('l', [ 4 ]);
        ('m', [ 2 ]);
      ];
    get_cor_test "cor keys of camel_camel_1" camel_camel_1
      [ 'a'; 'c'; 'e'; 'l'; 'm' ];
    get_misp_test "misp keys of camel_camel_1 is []" camel_camel_1 [];
    num_guess_test "num guesses of camel_camel_1 is 1" camel_camel_1 1;
  ]

(* guess multiple words *)
let camel_cloud_2 =
  move "cloud" camel_gme camel_smile_1 |> extract_result

let camel_cloud_2_tests =
  [
    last_guess_test "last guess of camel_cloud_2 is cloud" camel_cloud_2
      "cloud";
    prev_guesses_test
      "previous guesses of camel_cloud_2 is [cloud, smile]"
      camel_cloud_2 [ "cloud"; "smile" ];
    absent_letters_test "absent letters of camel_cloud_2" camel_cloud_2
      [ 'd'; 'i'; 'o'; 's'; 'u' ];
    misp_letters_test "misp letters of camel_cloud_2" camel_cloud_2
      [ ('e', [ 4 ]); ('l', [ 1; 3 ]); ('m', [ 1 ]) ];
    cor_letters_test "cor letters of camel_cloud_2" camel_cloud_2
      [ ('c', [ 0 ]) ];
    get_cor_test "cor keys of camel_cloud_2 is [c]" camel_cloud_2
      [ 'c' ];
    get_misp_test "misp keys of camel_cloud_2 is [e, l, m]"
      camel_cloud_2 [ 'e'; 'l'; 'm' ];
    num_guess_test "num guesses of camel_cloud_2 is 2" camel_cloud_2 2;
  ]

(* second guess is all misplaced *)
let camel_grind_2 =
  move "grind" camel_gme camel_smile_1 |> extract_result

let camel_grind_2_tests =
  [
    last_guess_test "last guess of camel_grind_2 is grind" camel_grind_2
      "grind";
    prev_guesses_test
      "previous guesses of camel_grind_2 is [grind, smile]"
      camel_grind_2 [ "grind"; "smile" ];
    absent_letters_test "absent letters of camel_grind_2" camel_grind_2
      [ 'd'; 'g'; 'i'; 'n'; 'r'; 's' ];
    misp_letters_test "misp letters of camel_grind_2" camel_grind_2
      [ ('e', [ 4 ]); ('l', [ 3 ]); ('m', [ 1 ]) ];
    cor_letters_test "cor letters of camel_grind_2" camel_grind_2 [];
    get_cor_test "cor keys of camel_grind_2 is []" camel_grind_2 [];
    get_misp_test "misp keys of camel_grind_2 is [e, l, m]"
      camel_grind_2 [ 'e'; 'l'; 'm' ];
    num_guess_test "num guesses of camel_grind_2 is 2" camel_grind_2 2;
  ]

(* test third guess *)
let camel_clock_3 =
  move "clock" camel_gme camel_grind_2 |> extract_result

let camel_clock_3_tests =
  [
    last_guess_test "last guess of camel_clock_3 is clock" camel_clock_3
      "clock";
    prev_guesses_test
      "previous guesses of camel_clock_3 is [clock, grind, smile]"
      camel_clock_3
      [ "clock"; "grind"; "smile" ];
    absent_letters_test "absent letters of camel_clock_3" camel_clock_3
      [ 'd'; 'g'; 'i'; 'k'; 'n'; 'o'; 'r'; 's' ];
    misp_letters_test "misp letters of camel_clock_3" camel_clock_3
      [ ('c', [ 3 ]); ('e', [ 4 ]); ('l', [ 1; 3 ]); ('m', [ 1 ]) ];
    cor_letters_test "cor letters of camel_clock_3" camel_clock_3
      [ ('c', [ 0 ]) ];
    get_cor_test "cor keys of camel_clock_3 is [c]" camel_clock_3
      [ 'c' ];
    get_misp_test "misp keys of camel_clock_3 is [e, l, m]"
      camel_clock_3 [ 'e'; 'l'; 'm' ];
    num_guess_test "num guesses of camel_clock_3 is 3" camel_clock_3 3;
  ]

(* test with repeat letters *)
let googs_gme = "googs" |> set_final_word gme
let googs_init = init_state googs_gme

(* test repeat letters with no repeat word *)
let googs_proud_1 = move "proud" googs_gme googs_init |> extract_result

let googs_proud_1_tests =
  [
    last_guess_test "last guess of googs_proud_1 is proud" googs_proud_1
      "proud";
    prev_guesses_test "previous guesses of googs_proud_1 is [proud]"
      googs_proud_1 [ "proud" ];
    absent_letters_test
      "absent letters of googs_proud_1 is [d, p, r, u]" googs_proud_1
      [ 'd'; 'p'; 'r'; 'u' ];
    misp_letters_test "misp letters of googs_proud_1 is []"
      googs_proud_1 [];
    cor_letters_test "cor letters of googs_proud_1" googs_proud_1
      [ ('o', [ 2 ]) ];
    get_cor_test "cor keys of googs_proud_1" googs_proud_1 [ 'o' ];
    get_misp_test "misp keys of googs_proud_1 is []" googs_proud_1 [];
    num_guess_test "num guesses of googs_proud_1 is 1" googs_proud_1 1;
  ]

(* test repeat letters with repeat word *)
let googs_bloop_1 = move "bloop" googs_gme googs_init |> extract_result

let googs_bloop_1_tests =
  [
    last_guess_test "last guess of googs_bloop_1 is bloop" googs_bloop_1
      "bloop";
    prev_guesses_test "previous guesses of googs_bloop_1 is [bloop]"
      googs_bloop_1 [ "bloop" ];
    absent_letters_test "absent letters of googs_bloop_1 is [b, l, p]"
      googs_bloop_1 [ 'b'; 'l'; 'p' ];
    misp_letters_test "misp letters of googs_bloop_1" googs_bloop_1
      [ ('o', [ 3 ]) ];
    cor_letters_test "cor letters of googs_bloop_1" googs_bloop_1
      [ ('o', [ 2 ]) ];
    get_cor_test "cor keys of googs_bloop_1" googs_bloop_1 [ 'o' ];
    get_misp_test "misp keys of googs_bloop_1 is []" googs_bloop_1 [];
    num_guess_test "num guesses of googs_bloop_1 is 1" googs_bloop_1 1;
  ]

(* test correct repeat letters with repeat word *)
let googs_booms_1 = move "booms" googs_gme googs_init |> extract_result

let googs_booms_1_tests =
  [
    last_guess_test "last guess of googs_booms_1 is booms" googs_booms_1
      "booms";
    prev_guesses_test "previous guesses of googs_booms_1 is [booms]"
      googs_booms_1 [ "booms" ];
    absent_letters_test "absent letters of googs_booms_1 is [b, m]"
      googs_booms_1 [ 'b'; 'm' ];
    misp_letters_test "misp letters of googs_booms_1" googs_booms_1 [];
    cor_letters_test "cor letters of googs_booms_1" googs_booms_1
      [ ('o', [ 1; 2 ]); ('s', [ 4 ]) ];
    get_cor_test "cor keys of googs_booms_1" googs_booms_1 [ 'o'; 's' ];
    get_misp_test "misp keys of googs_booms_1 is []" googs_booms_1 [];
    num_guess_test "num guesses of googs_booms_1 is 1" googs_booms_1 1;
  ]

(* second guess to repeated letters *)
let googs_pines_2a =
  move "pines" googs_gme googs_proud_1 |> extract_result

let googs_pines_2a_tests =
  [
    last_guess_test "last guess of googs_pines_2a is pines"
      googs_pines_2a "pines";
    prev_guesses_test
      "previous guesses of googs_pines_2a is [pines; proud]"
      googs_pines_2a [ "pines"; "proud" ];
    absent_letters_test "absent letters of googs_pines_2a"
      googs_pines_2a
      [ 'd'; 'e'; 'i'; 'n'; 'p'; 'r'; 'u' ];
    misp_letters_test "misp letters of googs_pines_2a is []"
      googs_pines_2a [];
    cor_letters_test "cor letters of googs_pines_2a" googs_pines_2a
      [ ('o', [ 2 ]); ('s', [ 4 ]) ];
    get_cor_test "cor keys of googs_pines_2a" googs_pines_2a
      [ 'o'; 's' ];
    get_misp_test "misp keys of googs_pines_2a is []" googs_pines_2a [];
    num_guess_test "num guesses of googs_pines_2a is 2" googs_pines_2a 2;
  ]

(* second guess to repeated letters with repeated first word *)
let googs_pines_2b =
  move "pines" googs_gme googs_bloop_1 |> extract_result

let googs_pines_2b_tests =
  [
    last_guess_test "last guess of googs_pines_2b is pines"
      googs_pines_2b "pines";
    prev_guesses_test
      "previous guesses of googs_pines_2b is [pines, bloop]"
      googs_pines_2b [ "pines"; "bloop" ];
    absent_letters_test "absent letters of googs_pines_2b"
      googs_pines_2b
      [ 'b'; 'e'; 'i'; 'n'; 'l'; 'p' ];
    misp_letters_test "misp letters of googs_pines_2b" googs_pines_2b
      [ ('o', [ 3 ]) ];
    cor_letters_test "cor letters of googs_pines_2b" googs_pines_2b
      [ ('o', [ 2 ]); ('s', [ 4 ]) ];
    get_cor_test "cor keys of googs_pines_2b" googs_pines_2b
      [ 'o'; 's' ];
    get_misp_test "misp keys of googs_pines_2b is []" googs_pines_2b [];
    num_guess_test "num guesses of googs_pines_2b is 2" googs_pines_2b 2;
  ]

(* third guess to repeated letters *)
let googs_grind_3 =
  move "grind" googs_gme googs_pines_2b |> extract_result

let googs_grind_3_tests =
  [
    last_guess_test "last guess of googs_grind_3 is grind" googs_grind_3
      "grind";
    prev_guesses_test
      "previous guesses of googs_grind_3 is [grind, pines, bloop]"
      googs_grind_3
      [ "grind"; "pines"; "bloop" ];
    absent_letters_test "absent letters of googs_grind_3" googs_grind_3
      [ 'b'; 'd'; 'e'; 'i'; 'n'; 'l'; 'p'; 'r' ];
    misp_letters_test "misp letters of googs_grind_3" googs_grind_3
      [ ('o', [ 3 ]) ];
    cor_letters_test "cor letters of googs_grind_3" googs_grind_3
      [ ('g', [ 0 ]); ('o', [ 2 ]); ('s', [ 4 ]) ];
    get_cor_test "cor keys of googs_grind_3" googs_grind_3
      [ 'g'; 'o'; 's' ];
    get_misp_test "misp keys of googs_grind_3 is []" googs_grind_3 [];
    num_guess_test "num guesses of googs_grind_3 is 3" googs_grind_3 3;
  ]

let state_tests =
  List.flatten
    [
      illegal_guess_tests;
      camel_init_tests;
      camel_proud_1_tests;
      camel_smile_1_tests;
      camel_calls_1_tests;
      camel_camel_1_tests;
      camel_cloud_2_tests;
      camel_grind_2_tests;
      camel_clock_3_tests;
      googs_proud_1_tests;
      googs_bloop_1_tests;
      googs_booms_1_tests;
      googs_pines_2a_tests;
      googs_pines_2b_tests;
      googs_grind_3_tests;
    ]
