open OUnit2
open Wordle
open Game

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"
let gme = from_txt filename_g filename_w

(********************************************************************
   Testing Game
 ********************************************************************)

let from_txt_test_guess
    (name : string)
    (txt_guess : string)
    (txt_word : string)
    (index : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  let real = from_txt txt_guess txt_word in
  assert_equal expected_output (List.nth (get_guesslist real) index)

let from_txt_test_word
    (name : string)
    (txt_guess : string)
    (txt_word : string)
    (index : int)
    (expected_output : string) : test =
  name >:: fun _ ->
  let real = from_txt txt_guess txt_word in
  assert_equal expected_output (List.nth (get_wordlist real) index)

let is_valid_guess_test
    (name : string)
    (t : Game.t)
    (str : string)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (is_valid_guess t str)

let rec final_word_prop_test (count : int) (limit : int) =
  if count = limit then true
  else
    filename_w |> from_txt filename_g |> final_word |> String.length = 5
    && final_word_prop_test (count + 1) limit

(* In testing from_txt, we will also test get_wordlist, get_guesslist,
   txt_to_lst *)
let from_txt_tests =
  [
    from_txt_test_guess "start word of guess list is aahed" filename_g
      filename_w 0 "aahed";
    from_txt_test_guess "fifth word of guess list is abaca" filename_g
      filename_w 4 "abaca";
    from_txt_test_guess "100th word of guess list is adsum" filename_g
      filename_w 99 "adsum";
    from_txt_test_guess "second to last word of guess list is zesty"
      filename_g filename_w
      ((gme |> get_guesslist |> List.length) - 2)
      "zesty";
    from_txt_test_guess "last word of guess list is zonal" filename_g
      filename_w
      ((gme |> get_guesslist |> List.length) - 1)
      "zonal";
    from_txt_test_word "start word of word list is aback" filename_g
      filename_w 0 "aback";
    from_txt_test_word "seventh word of word list is abide" filename_g
      filename_w 6 "abide";
    from_txt_test_word "100th word of word list is apron" filename_g
      filename_w 99 "apron";
    from_txt_test_word "last word of word list is zonal" filename_g
      filename_w
      ((gme |> get_wordlist |> List.length) - 1)
      "zonal";
  ]

let is_valid_guess_tests =
  [
    is_valid_guess_test "raise is a valid guess from guess list" gme
      "raise" true;
    is_valid_guess_test "supra is a valid guess from guess list" gme
      "supra" true;
    is_valid_guess_test "supes is a valid guess from guess list" gme
      "supes" true;
    is_valid_guess_test "wedge is a valid guess from guess list" gme
      "wedge" true;
    is_valid_guess_test "aaaaa is not a valid guess from guess list" gme
      "aaaaa" false;
    is_valid_guess_test "hi is not a valid guess from guess list" gme
      "hi" false;
    is_valid_guess_test "longword is not a valid guess from guess list"
      gme "longword" false;
    is_valid_guess_test "hehe! is not a valid guess from guess list" gme
      "hehe!" false;
    is_valid_guess_test "_____ is not a valid guess from guess list" gme
      "_____" false;
  ]

let final_word_prop_tests =
  [
    ( "testing final word property 50 times" >:: fun _ ->
      assert_equal true (final_word_prop_test 0 50) );
  ]

let set_final_word_tests =
  [
    ( "setting final word to hello" >:: fun _ ->
      assert_equal "hello" ("hello" |> set_final_word gme |> final_word)
    );
    ( "setting final word to wheat" >:: fun _ ->
      assert_equal "wheat" ("wheat" |> set_final_word gme |> final_word)
    );
    ( "setting final word to googs" >:: fun _ ->
      assert_equal "googs" ("googs" |> set_final_word gme |> final_word)
    );
    ( "setting final word to aaaaa, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "aaaaa" |> set_final_word gme) );
    ( "setting final word to hi, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "aaaaa" |> set_final_word gme) );
    ( "setting final word to 12345, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "12345" |> set_final_word gme) );
    ( "setting final word to special characters, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "base*" |> set_final_word gme) );
    ( "setting final word to 5 spaces, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "     " |> set_final_word gme) );
    ( "setting final word to a six letter word, invalid" >:: fun _ ->
      assert_raises (Failure "invalid word") (fun () ->
          "peanut" |> set_final_word gme) );
  ]

let game_tests =
  List.flatten
    [
      from_txt_tests;
      is_valid_guess_tests;
      final_word_prop_tests;
      set_final_word_tests;
    ]