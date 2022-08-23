open OUnit2
open Wordle
open Entropy

let sort lst = List.sort compare lst
let permutations = generate_perms 0

let log2_test (name : string) (input : float) (expected_output : float)
    =
  name >:: fun _ -> assert_equal expected_output (log2 input)

let abstract_to_real_a_test
    (name : string)
    (indices : int list)
    (word : string)
    (expected_output : char list) : test =
  name >:: fun _ ->
  assert_equal (sort expected_output)
    (word |> abstract_to_real_a indices |> sort)

let illegal_abs_to_real_a_test
    (name : string)
    (indices : int list)
    (word : string) =
  name >:: fun _ ->
  assert_raises (Invalid_argument "index out of bounds") (fun () ->
      abstract_to_real_a indices word)

let abstract_to_real_b_test
    (name : string)
    (indices : int list)
    (word : string)
    (expected_output : (char * int list) list) : test =
  name >:: fun _ ->
  assert_equal (sort expected_output)
    (word |> abstract_to_real_b indices |> sort)

let illegal_abs_to_real_b_test
    (name : string)
    (indices : int list)
    (word : string) =
  name >:: fun _ ->
  assert_raises (Invalid_argument "index out of bounds") (fun () ->
      abstract_to_real_b indices word)

let generate_perms_test (name : string) (target_array : int list array)
    : test =
  name >:: fun _ ->
  assert_equal target_array
    (List.find
       (fun x -> if x = target_array then true else false)
       permutations)

let illegal_perms_tests (name : string) (target_array : int list array)
    =
  name >:: fun _ ->
  assert_raises Not_found (fun () ->
      List.find
        (fun x -> if x = target_array then true else false)
        permutations)

let log2_tests =
  [
    log2_test "log2 of 4 is 2" 4.0 2.0;
    log2_test "log2 of 16 is 4" 16.0 4.0;
    log2_test "log2 of 128 is 7" 128.0 7.0;
    log2_test "log2 of 1024 is 10" 1024.0 10.0;
    log2_test "log2 of 1 is 0" 1.0 0.0;
  ]

let abstract_to_real_a_tests =
  [
    abstract_to_real_a_test "find [1;2] characters of hello" [ 1; 2 ]
      "hello" [ 'e'; 'l' ];
    abstract_to_real_a_test "find [0-4] characters of hello"
      [ 0; 1; 2; 3; 4 ] "hello"
      [ 'e'; 'h'; 'l'; 'l'; 'o' ];
    abstract_to_real_a_test "find [0] characters of hello" [ 0 ] "hello"
      [ 'h' ];
    abstract_to_real_a_test "find [0] characters of hi" [ 0 ] "hi"
      [ 'h' ];
    abstract_to_real_a_test "find [6; 7] characters of aphrodite"
      [ 6; 7 ] "aphrodite" [ 'i'; 't' ];
    abstract_to_real_a_test "find [0; 1; 2] characters of '     '"
      [ 0; 1; 2 ] "     " [ ' '; ' '; ' ' ];
    abstract_to_real_a_test "find [0; 9] characters of superlongword"
      [ 0; 9 ] "superlongword" [ 's'; 'w' ];
    abstract_to_real_a_test "find [0-12] characters of superlongword"
      [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ]
      "superlongword"
      [
        'd'; 'e'; 'g'; 'l'; 'n'; 'o'; 'o'; 'p'; 'r'; 'r'; 's'; 'u'; 'w';
      ];
  ]

let illegal_abs_to_real_a_tests =
  [
    illegal_abs_to_real_a_test "illegal to have a negative index" [ -1 ]
      "david";
    illegal_abs_to_real_a_test "illegal to have a high index" [ 100 ]
      "david";
    illegal_abs_to_real_a_test "illegal to have index equal to length"
      [ 5 ] "david";
    illegal_abs_to_real_a_test "negative index with valid indices"
      [ -1; 1; 2 ] "david";
    illegal_abs_to_real_a_test "invalid index with valid indices"
      [ 1; 2; 5 ] "david";
  ]

let abstract_to_real_b_tests =
  [
    abstract_to_real_b_test "find [1;2] characters and indices of hello"
      [ 1; 2 ] "hello"
      [ ('e', [ 1 ]); ('l', [ 2; 3 ]) ];
    abstract_to_real_b_test "find [0-4] characters and indices of hello"
      [ 0; 1; 2; 3; 4 ] "hello"
      [
        ('e', [ 1 ]);
        ('h', [ 0 ]);
        ('l', [ 2; 3 ]);
        ('l', [ 2; 3 ]);
        ('o', [ 4 ]);
      ];
    abstract_to_real_b_test "find [0] characters and indices of hello"
      [ 0 ] "hello"
      [ ('h', [ 0 ]) ];
    abstract_to_real_b_test "find [0] characters and indices of hi"
      [ 0 ] "hi"
      [ ('h', [ 0 ]) ];
    abstract_to_real_b_test
      "find [0] character of all repeating letters" [ 0 ] "mmmmmmmmmm"
      [ ('m', [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]) ];
    abstract_to_real_b_test "find [9;10] character of verylongword"
      [ 9; 10 ] "verylongword"
      [ ('o', [ 5; 9 ]); ('r', [ 2; 10 ]) ];
    abstract_to_real_b_test "find [0-11] character of verylongword"
      [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11 ]
      "verylongword"
      [
        ('d', [ 11 ]);
        ('e', [ 1 ]);
        ('g', [ 7 ]);
        ('l', [ 4 ]);
        ('o', [ 5; 9 ]);
        ('o', [ 5; 9 ]);
        ('n', [ 6 ]);
        ('r', [ 2; 10 ]);
        ('r', [ 2; 10 ]);
        ('v', [ 0 ]);
        ('w', [ 8 ]);
        ('y', [ 3 ]);
      ];
  ]

let illegal_abs_to_real_b_tests =
  [
    illegal_abs_to_real_b_test "illegal to have a negative index b"
      [ -1 ] "negative";
    illegal_abs_to_real_b_test "illegal to have a high index b" [ 1000 ]
      "highindex";
    illegal_abs_to_real_b_test "illegal to have index equal to length b"
      [ 5 ] "index";
    illegal_abs_to_real_b_test "negative index with valid indices b"
      [ -1; 1; 2 ] "andy";
    illegal_abs_to_real_b_test "invalid index with valid indices b"
      [ 1; 2; 5 ] "anson";
  ]

let generate_perms_tests =
  [
    generate_perms_test "[| [0;1;2;3;4]; []; [] |] exists in perms"
      [| [ 0; 1; 2; 3; 4 ]; []; [] |];
    generate_perms_test "[| []; []; [0;1;2;3;4] |] exists in perms"
      [| []; []; [ 0; 1; 2; 3; 4 ] |];
    generate_perms_test "[| [0]; [1]; [2;3;4] |] exists in perms"
      [| [ 0 ]; [ 1 ]; [ 2; 3; 4 ] |];
    generate_perms_test "[| [0; 4]; [1]; [2;3] |] exists in perms"
      [| [ 0; 4 ]; [ 1 ]; [ 2; 3 ] |];
    generate_perms_test "[| [4]; [3]; [0;1;2] |] exists in perms"
      [| [ 4 ]; [ 3 ]; [ 0; 1; 2 ] |];
    generate_perms_test "[| [1;3]; [2]; [0;4] |] exists in perms"
      [| [ 1; 3 ]; [ 2 ]; [ 0; 4 ] |];
    generate_perms_test "[| [2;3]; [4]; [0;1] |] exists in perms"
      [| [ 2; 3 ]; [ 4 ]; [ 0; 1 ] |];
    generate_perms_test "[| [1]; [2]; [0;3;4] |] exists in perms"
      [| [ 1 ]; [ 2 ]; [ 0; 3; 4 ] |];
  ]

let illegal_perms_tests =
  [
    illegal_perms_tests "[| []; []; [] |] does not exist in perms"
      [| []; []; [] |];
    illegal_perms_tests
      "[| [0;1;2;3;4;5]; []; [] |] does not exist in perms"
      [| [ 0; 1; 2; 3; 4; 5 ]; []; [] |];
    illegal_perms_tests
      "[| [0;1;2;4]; []; [] |] does not exist in perms"
      [| [ 0; 1; 2; 4 ]; []; [] |];
    illegal_perms_tests "[| [10]; []; [] |] does not exist in perms"
      [| [ 10 ]; []; [] |];
    illegal_perms_tests
      "[| [0]; [0;1;2;3;4]; [] |] does not exist in perms"
      [| [ 0 ]; [ 0; 1; 2; 3; 4 ]; [] |];
    illegal_perms_tests "[| [1]; [1]; [1] |] does not exist in perms"
      [| [ 1 ]; [ 1 ]; [ 1 ] |];
    illegal_perms_tests "[| [0]; [6]; [1] |] does not exist in perms"
      [| [ 0 ]; [ 6 ]; [ 1 ] |];
    illegal_perms_tests "[| [2]; [3]; [2] |] does not exist in perms"
      [| [ 2 ]; [ 3 ]; [ 2 ] |];
  ]

let entropy_tests =
  List.flatten
    [
      log2_tests;
      abstract_to_real_a_tests;
      illegal_abs_to_real_a_tests;
      abstract_to_real_b_tests;
      illegal_abs_to_real_b_tests;
      generate_perms_tests;
      illegal_perms_tests;
    ]
