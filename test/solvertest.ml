open OUnit2
open Wordle
open Solver
open State
open Game

(********************************************************************
   Testing Solver
 ********************************************************************)

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"
let explode s = List.init (String.length s) (String.get s)

let extract = function
  | Legal t -> t
  | Illegal -> failwith "???"

let remove_absents_test
    (name : string)
    (absents : char list)
    (word_list : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_absents absents word_list)

let remove_misplaced_test
    (name : string)
    (misplaced : (char * int list) list)
    (word_list : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_misplaced misplaced word_list)

let remove_nc_test
    (name : string)
    (correct : (char * int list) list)
    (word_list : string list)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (remove_nc correct word_list)

let narrow_test
    (name : string)
    (guess : string)
    (state : State.t)
    (word_list : string list) : test =
  name >:: fun _ ->
  assert_equal (List.mem guess (narrow_list state word_list)) false

let lookup_freq_test
    (name : string)
    (freq : letter_freq)
    (letter : char)
    (pos : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (lookup_freq freq letter pos)

let incr_freq_test
    (name : string)
    (freq : letter_freq)
    (letter : char)
    (pos : int)
    (expected_output : unit) : test =
  name >:: fun _ ->
  assert_equal expected_output (incr_freq freq letter pos)

let count_letters_test
    (name : string)
    (freq : letter_freq)
    (word : string)
    (expected_output : letter_freq) : test =
  name >:: fun _ ->
  assert_equal expected_output (count_letters freq word)

let gen_freq_map_test
    (name : string)
    (word_list : string list)
    (expected_output : letter_freq) : test =
  name >:: fun _ ->
  assert_equal expected_output (gen_freq_map word_list)

let word_score_test
    (name : string)
    (freq : letter_freq)
    (word : string)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (word_score freq word)
    ~printer:string_of_int

let manual_narrow_test
    (name : string)
    (absents : char list)
    (misplaced : (char * int list) list)
    (correct : (char * int list) list)
    (word_list : string list)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (List.length
       (word_list |> remove_absents absents
       |> remove_misplaced misplaced
       |> remove_nc correct))
    ~printer:string_of_int

let strings_original = [ "power"; "cower"; "dowry"; "moper"; "green" ]
let strings_original2 = [ "water"; "apple"; "grill"; "david"; "andyy" ]
let strings_original3 = [ "david"; "eugen"; "andyy"; "anson" ]
let strings_original4 = [ "wordl"; "worde" ]
let absents_1 = [ 'm'; 'c' ]
let absents_2 = [ 'o' ]
let absents_3 = [ 'l' ]
let absents_4 = [ 'w'; 'i' ]
let absents_5 = [ 'c' ]
let absents_6 = [ 'd' ]
let absents_7 = [ 'a'; 'y' ]
let absents_8 = [ 'e' ]
let absents_9 = [ 'w' ]
let strings_a_1 = [ "power"; "dowry"; "green" ]
let strings_a_2 = [ "green" ]
let strings_a_3 = [ "water"; "david"; "andyy" ]
let strings_a_4 = [ "apple"; "andyy" ]
let strings_a_6 = [ "eugen"; "anson" ]
let strings_a_7 = [ "eugen" ]
let strings_a_8 = [ "wordl" ]
let strings_a_9 = []
let misplaced_1 = [ ('r', [ 4 ]) ]
let strings_m_1 = [ "dowry"; "green" ]
let misplaced_2 = [ ('i', [ 2 ]) ]
let strings_m_2 = [ "david" ]
let misplaced_3 = [ ('e', [ 2 ]) ]
let strings_m_3 = [ "power"; "cower"; "moper" ]
let misplaced_4 = [ ('r', [ 4 ]) ]
let strings_m_4 = [ "grill" ]
let misplaced_5 = [ ('a', [ 1 ]) ]
let strings_m_5 = [ "andyy"; "anson" ]
let misplaced_6 = [ ('n', [ 1 ]) ]
let strings_m_6 = [ "eugen" ]
let misplaced_7 = [ ('d', [ 0 ]) ]
let strings_m_7 = [ "andyy" ]
let misplaced_8 = [ ('w', [ 0 ]) ]
let strings_m_8 = []
let misplaced_9 = [ ('e', [ 4 ]) ]
let strings_m_9 = []
let correct_1 = [ ('e', [ 2; 3 ]) ]
let correct_2 = [ ('o', [ 1 ]); ('e', [ 3 ]) ]
let correct_3 = [ ('o', [ 1 ]) ]
let correct_4 = [ ('a', [ 0 ]) ]
let correct_5 = [ ('a', [ 0 ]); ('e', [ 4 ]) ]
let correct_6 = [ ('r', [ 4 ]) ]
let correct_7 = [ ('a', [ 0 ]) ]
let correct_8 = [ ('d', [ 0 ]) ]
let correct_9 = [ ('l', [ 4 ]) ]
let strings_c_1 = [ "green" ]
let strings_c_2 = [ "power"; "cower"; "moper" ]
let strings_c_3 = [ "power"; "cower"; "dowry"; "moper" ]
let strings_c_4 = [ "apple"; "andyy" ]
let strings_c_5 = [ "apple" ]
let strings_c_6 = [ "water" ]
let strings_c_7 = [ "andyy"; "anson" ]
let strings_c_8 = [ "david" ]
let strings_c_9 = [ "wordl" ]
let gme = from_txt filename_g filename_w
let state0 = init_state gme
let wordlist = Game.get_guesslist gme
let init_freq1 = init_freq []
let init_freq2 = init_freq []
let init_freq3 = init_freq []
let init_freq4 = init_freq []
let init_freq5 = init_freq []
let init_freq6 = init_freq []
let init_freq7 = init_freq []
let paper_freq = count_letters init_freq1 "paper"

let pp_freq =
  let ss = count_letters init_freq2 "paste" in
  count_letters ss "paper"

let pps_freq =
  let a = count_letters init_freq3 "paste" in
  let b = count_letters a "paper" in
  count_letters b "shift"

let ppss_freq =
  let c = count_letters init_freq4 "chalk" in
  let a = count_letters c "paste" in
  let b = count_letters a "paper" in
  count_letters b "shift"

let ppsss_freq =
  let d = count_letters init_freq5 "cruel" in
  let c = count_letters d "chalk" in
  let a = count_letters c "paste" in
  let b = count_letters a "paper" in
  count_letters b "shift"

let ppssst_freq =
  let e = count_letters init_freq6 "aaaaa" in
  let d = count_letters e "cruel" in
  let c = count_letters d "chalk" in
  let a = count_letters c "paste" in
  let b = count_letters a "paper" in
  count_letters b "shift"

let p_freq =
  let f = count_letters init_freq7 "zzzzz" in
  let e = count_letters f "aaaaa" in
  let d = count_letters e "cruel" in
  let c = count_letters d "chalk" in
  let a = count_letters c "paste" in
  let b = count_letters a "paper" in
  count_letters b "shift"

let mn_absents_1 = [ 'w'; 'y' ]
let mn_misplaced_1 = [ ('e', [ 1 ]); ('a', [ 2 ]); ('r', [ 3 ]) ]
let mn_correct_1 = []
let mn_absents_2 = [ 'v'; 'd' ]
let mn_misplaced_2 = [ ('a', [ 1 ]); ('r', [ 2 ]) ]
let mn_correct_2 = []
let mn_absents_3 = [ 'b'; 'f' ]
let mn_misplaced_3 = [ ('s', [ 4 ]); ('r', [ 2 ]) ]

let mn_correct_4 =
  [
    ('b', [ 0 ]); ('a', [ 1 ]); ('s', [ 2 ]); ('e', [ 3 ]); ('d', [ 4 ]);
  ]

let mn_absents_4 = [ 'c'; 'g' ]
let mn_misplaced_4 = []

let mn_correct_5 =
  [
    ('*', [ 0 ]); ('a', [ 1 ]); ('s', [ 2 ]); ('e', [ 3 ]); ('d', [ 4 ]);
  ]

let mn_absents_5 = [ 'z'; '*' ]
let mn_misplaced_5 = []
let mn_absents_6 = [ ' '; '*' ]
let mn_misplaced_6 = []

let solver_tests =
  [
    remove_absents_test
      "absent_letters absents1 strings_original is strings_a_1"
      absents_1 strings_original strings_a_1;
    remove_absents_test
      "absent_letters absents2 strings_original is strings_a_2"
      absents_2 strings_original strings_a_2;
    remove_absents_test
      "absent_letters absents3 strings_original2 is strings_a_3"
      absents_3 strings_original2 strings_a_3;
    remove_absents_test
      "absent_letters absents5 strings_original2 is strings_a_5"
      absents_5 strings_original2 strings_original2;
    remove_absents_test
      "absent_letters absents4 strings_original2 is strings_a_4"
      absents_4 strings_original2 strings_a_4;
    remove_absents_test
      "absent_letters absents7 strings_original2 is strings_a_7"
      absents_7 strings_original3 strings_a_7;
    remove_absents_test
      "absent_letters absents6 strings_original3 is strings_a_6"
      absents_6 strings_original3 strings_a_6;
    remove_absents_test
      "absent_letters absents8 strings_original4 is strings_a_8"
      absents_8 strings_original4 strings_a_8;
    remove_absents_test
      "absent_letters absents9 strings_original4 is strings_a_9"
      absents_9 strings_original4 strings_a_9;
    remove_misplaced_test
      "remove_misplaced misplaced_1 strings_original is strings_m_1"
      misplaced_1 strings_original strings_m_1;
    remove_misplaced_test
      "remove_misplaced misplaced_3 strings_original is strings_m_3"
      misplaced_3 strings_original strings_m_3;
    remove_misplaced_test
      "remove_misplaced misplaced_2 strings_original2 is strings_m_2"
      misplaced_2 strings_original2 strings_m_2;
    remove_misplaced_test
      "remove_misplaced misplaced_4 strings_original2 is strings_m_4"
      misplaced_4 strings_original2 strings_m_4;
    remove_misplaced_test
      "remove_misplaced misplaced_5 strings_original3 is strings_m_5"
      misplaced_5 strings_original3 strings_m_5;
    remove_misplaced_test
      "remove_misplaced misplaced_6 strings_original3 is strings_m_6"
      misplaced_6 strings_original3 strings_m_6;
    remove_misplaced_test
      "remove_misplaced misplaced_7 strings_original3 is strings_m_7"
      misplaced_7 strings_original3 strings_m_7;
    remove_misplaced_test
      "remove_misplaced misplaced_8 strings_original4 is strings_m_8"
      misplaced_8 strings_original4 strings_m_8;
    remove_misplaced_test
      "remove_misplaced misplaced_9 strings_original4 is strings_m_9"
      misplaced_9 strings_original4 strings_m_9;
    remove_nc_test
      "remove_correct correct_1 strings_original is strings_c_1"
      correct_1 strings_original strings_c_1;
    remove_nc_test
      "remove_correct correct_2 strings_original is strings_c_2"
      correct_2 strings_original strings_c_2;
    remove_nc_test
      "remove_correct correct_3 strings_original is strings_c_3"
      correct_3 strings_original strings_c_3;
    remove_nc_test
      "remove_correct correct_4 strings_original2 is strings_c_4"
      correct_4 strings_original2 strings_c_4;
    remove_nc_test
      "remove_correct correct_5 strings_original2 is strings_c_5"
      correct_5 strings_original2 strings_c_5;
    remove_nc_test
      "remove_correct correct_6 strings_original2 is strings_c_6"
      correct_6 strings_original2 strings_c_6;
    remove_nc_test
      "remove_correct correct_7 strings_original3 is strings_c_7"
      correct_7 strings_original3 strings_c_7;
    remove_nc_test
      "remove_correct correct_8 strings_original3 is strings_c_8"
      correct_8 strings_original3 strings_c_8;
    remove_nc_test
      "remove_correct correct_9 strings_original4 is strings_c_9"
      correct_9 strings_original4 strings_c_9;
    narrow_test "guessing recur should remove recur from narrowed list"
      "recur"
      (State.move "recur" gme state0 |> extract)
      wordlist;
    narrow_test "guessing raise should remove raise from narrowed list"
      "raise"
      (State.move "raise" gme state0 |> extract)
      wordlist;
    narrow_test "guessing mount should remove mount from narrowed list"
      "mount"
      (State.move "mount" gme state0 |> extract)
      wordlist;
    narrow_test "guessing hello should remove mount from narrowed list"
      "hello"
      (State.move "hello" gme state0 |> extract)
      wordlist;
    narrow_test
      "guessing hello again should not remove hello from narrowed list"
      "hello"
      (State.move "hello" gme state0 |> extract)
      wordlist;
    narrow_test
      "guessing mount again should remove hello from narrowed list"
      "mount"
      (State.move "mount" gme state0 |> extract)
      wordlist;
    lookup_freq_test "p exists once in pos 0" paper_freq 'p' 0 1;
    lookup_freq_test "p does no exist in pos 1" paper_freq 'p' 1 0;
    lookup_freq_test "p does exist once in pos 2" paper_freq 'p' 2 1;
    lookup_freq_test "a exists once in pos 1" paper_freq 'a' 1 1;
    lookup_freq_test "e exists once in pos 3" paper_freq 'e' 3 1;
    lookup_freq_test "r exists once in pos 4" paper_freq 'r' 4 1;
    lookup_freq_test "p now exists twice in pos 0 with paper and paste"
      pp_freq 'p' 0 2;
    lookup_freq_test "a now exists twice in pos 1 with paper and paste"
      pp_freq 'a' 1 2;
    lookup_freq_test "s now exists once in pos 2 with paper and paste"
      pp_freq 's' 2 1;
    lookup_freq_test "q now exists none in pos 4 with paper and paste"
      pp_freq 'q' 4 0;
    gen_freq_map_test "[paper] is equivalent to paper for count_letters"
      [ "paper" ] paper_freq;
    gen_freq_map_test
      "[paper, paste] is equivalent to paper and paste for \
       count_letters"
      [ "paper"; "paste" ] pp_freq;
    gen_freq_map_test
      "[paper, paste, shift] is equivalent to shift, paper and paste \
       for count_letters"
      [ "shift"; "paper"; "paste" ]
      pps_freq;
    gen_freq_map_test
      "[paper, paste, shift, chalk] is equivalent to chalk, shift, \
       paper and paste for count_letters"
      [ "chalk"; "shift"; "paper"; "paste" ]
      ppss_freq;
    gen_freq_map_test
      "[paper, paste, shift, chalk, cruel] is equivalent to cruel, \
       chalk, shift, paper and paste for count_letters"
      [ "cruel"; "chalk"; "shift"; "paper"; "paste" ]
      ppsss_freq;
    gen_freq_map_test
      "[paper, paste, shift, chalk, cruel, aaaaa] is equivalent to \
       aaaaa, cruel, chalk, shift, paper and paste for count_letters"
      [ "aaaaa"; "cruel"; "chalk"; "shift"; "paper"; "paste" ]
      ppssst_freq;
    gen_freq_map_test
      "[paper, paste, shift, chalk, cruel, aaaaa, zzzzz] is equivalent \
       to zzzzz, aaaaa, cruel, chalk, shift, paper and paste for \
       count_letters"
      [ "zzzzz"; "aaaaa"; "cruel"; "chalk"; "shift"; "paper"; "paste" ]
      p_freq;
    word_score_test "the score of paste in pps_freq is 35" pps_freq
      "paste" 35;
    word_score_test "the score of paper in pps_freq is 28" pps_freq
      "paper" 28;
    word_score_test "the score of no string in pps_freq is 0" pps_freq
      "" 0;
    word_score_test "the score of fried in pps_freq is 10" pps_freq
      "fried" 10;
    word_score_test "the score of fried in pp_freq is 5" pp_freq "fried"
      5;
    word_score_test "the score of chimp in ppss_freq is 20" ppss_freq
      "chimp" 20;
    word_score_test "the score of paper in ppss_freq is 28" ppss_freq
      "paper" 28;
    word_score_test "the score of chimp in ppsss_freq is 25" ppsss_freq
      "chimp" 25;
    word_score_test "the score of zzzzz in ppsss_freq is 0" ppsss_freq
      "zzzzz" 0;
    word_score_test "the score of abcde in ppsss_freq is 5" ppsss_freq
      "abcde" 5;
    manual_narrow_test
      "manually narrowing the wordlist with mn1 gives a list of length \
       308"
      mn_absents_1 mn_misplaced_1 mn_correct_1 (get_guesslist gme) 308;
    manual_narrow_test
      "manually narrowing the wordlist with mn3 gives a list of length \
       303"
      mn_absents_3 mn_misplaced_3 mn_correct_2 (get_guesslist gme) 303;
    manual_narrow_test
      "manually narrowing the wordlist with mn4 gives a list of length \
       1"
      mn_absents_4 mn_misplaced_4 mn_correct_4 (get_guesslist gme) 1;
    manual_narrow_test
      "manually narrowing the wordlist with mn5 gives a list of length \
       5"
      mn_absents_5 mn_misplaced_5 mn_correct_5 (get_guesslist gme) 0;
    manual_narrow_test
      "manually narrowing the wordlist with mn6 gives a list of length \
       12972"
      mn_absents_6 mn_misplaced_6 mn_correct_2 (get_guesslist gme) 12972;
  ]
