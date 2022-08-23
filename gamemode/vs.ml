open Wordle
open Game
open State
open Solver

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"

let clear = function
  | x -> Sys.command "clear" + x

let print_y str = ANSITerminal.print_string [ ANSITerminal.yellow ] str
let print_g str = ANSITerminal.print_string [ ANSITerminal.green ] str
let print_w str = ANSITerminal.print_string [ ANSITerminal.white ] str
let string_of_char = String.make 1

let rec print_keys_helper misp corr wrong str =
  match str with
  | [] -> print_endline ""
  | h :: t ->
      let h_lower = Char.lowercase_ascii h in
      if List.mem h_lower misp then print_y (string_of_char h)
      else if List.mem h_lower corr then print_g (string_of_char h)
      else print_w (string_of_char h);
      print_char ' ';
      print_keys_helper misp corr wrong t

let explode s = List.init (String.length s) (String.get s)

let print_keys st =
  let misp = get_misp st in
  let corr = get_cor st in
  let wrong = absent_letters in
  print_keys_helper misp corr wrong (explode "QWERTYUIOP");
  print_string " ";
  print_keys_helper misp corr wrong (explode "ASDFGHJKL");
  print_string "  ";
  print_keys_helper misp corr wrong (explode "ZXCVBNM")

let conv_str ch_lst =
  String.uppercase_ascii
    (String.concat " " (List.map (String.make 1) ch_lst))

let rec inline_helper gme h c =
  match (h, c) with
  | [], [] -> ()
  | h1 :: t, [] ->
      print_word h1 (final_word gme);
      print_string "\t\t?????";
      print_endline "";
      inline_helper gme t c
  | [], _ :: t ->
      print_endline "     \t\t?????";
      inline_helper gme h t
  | h1 :: t1, h2 :: t2 ->
      print_word h1 (final_word gme);
      print_endline "\t\t?????";
      inline_helper gme t1 t2

and inline_helper_r gme h c =
  match (h, c) with
  | [], [] -> ()
  | h1 :: t, [] ->
      print_word h1 (final_word gme);
      print_string "\t\t";
      print_endline "";
      inline_helper_r gme t c
  | [], h2 :: t ->
      print_string "     \t\t";
      print_word h2 (final_word gme);
      print_endline "";
      inline_helper_r gme h t
  | h1 :: t1, h2 :: t2 ->
      print_word h1 (final_word gme);
      print_string "\t\t";
      print_word h2 (final_word gme);
      print_endline "";
      inline_helper_r gme t1 t2

let rec p_helper gme = function
  | [] -> ()
  | h :: t ->
      print_word h (final_word gme);
      print_endline "";
      p_helper gme t

let printer gme hst cst finished reveal =
  print_endline "";
  print_endline " YOU \t\t CPU ";
  if reveal then
    inline_helper_r gme
      (List.rev (previous_guesses hst))
      (List.rev (previous_guesses cst))
  else
    inline_helper gme
      (List.rev (previous_guesses hst))
      (List.rev (previous_guesses cst));
  print_endline "";
  if not finished then (
    print_keys hst;
    print_y "\nMisplaced Letters: ";
    print_y (conv_str (get_misp hst));
    print_g "\nCorrect Letters: ";
    print_g (conv_str (get_cor hst));
    print_w "\nWrong Letters: ";
    print_w (conv_str (List.sort_uniq compare (absent_letters hst))))
  else ();
  print_endline ""

let end_game hlg clg hng cng final =
  if hlg = final && clg <> final then print_g "You Win!"
  else if clg = final && hlg <> final then
    ANSITerminal.print_string [ ANSITerminal.red ] "You Lose!"
  else if clg = final && hlg = final && hng < cng then
    print_g "You Win!"
  else if clg = final && hlg = final && cng < hng then
    ANSITerminal.print_string [ ANSITerminal.red ] "You Lose!"
  else print_string "Tie!";
  print_endline ""

let rec play_wordle gme hst cst solver hfin cfin =
  match (hfin, cfin) with
  | true, true -> (
      ignore (clear 0);
      printer gme hst cst true true;
      let final = final_word gme in
      match (previous_guesses hst, previous_guesses cst) with
      | h1 :: _, h2 :: _ ->
          end_game h1 h2
            (previous_guesses hst |> List.length)
            (previous_guesses cst |> List.length)
            final
      | _ -> failwith "why empty")
  | false, false -> (
      print_endline "\n\nType your guess: ";
      let user_guess = String.lowercase_ascii (read_line ()) in
      ignore (clear 0);
      let next_cst, c_cor =
        let guess = Solver.solve cst gme solver in
        match State.move guess gme cst with
        | Legal t -> (t, guess = final_word gme)
        | Illegal -> failwith "???"
      in

      let correct = user_guess = final_word gme in
      let cfin' = c_cor || num_guess cst = 5 in
      match move user_guess gme hst with
      | Legal t ->
          printer gme t next_cst correct false;
          if correct then (
            print_w "\n\nYou have guessed the correct word: ";
            print_g (String.uppercase_ascii user_guess);
            play_wordle gme t next_cst solver true cfin')
          else if num_guess hst = 5 then (
            print_w "\n\nNo more guesses left. The correct word was: ";
            print_g (String.uppercase_ascii (final_word gme));
            print_endline "";
            play_wordle gme t next_cst solver true cfin')
          else play_wordle gme t next_cst solver hfin cfin'
      | Illegal ->
          ignore (clear 0);
          print_endline "That is an invalid guess. Pick another word.\n";
          printer gme hst cst false false;
          play_wordle gme hst cst solver false false)
  | true, false ->
      print_endline "\n\nComputer is still guessing...";
      let cguess = Solver.solve cst gme solver in
      let new_st =
        match State.move cguess gme cst with
        | Legal t -> t
        | Illegal -> failwith "???"
      in
      let correct = cguess = final_word gme in
      printer gme hst new_st correct true;
      if correct || num_guess cst = 5 then
        play_wordle gme hst new_st solver true true
      else play_wordle gme hst new_st solver true false
  | false, true -> (
      let user_guess = String.lowercase_ascii (read_line ()) in
      let correct = user_guess = final_word gme in
      ignore (clear 0);
      match move user_guess gme hst with
      | Legal t ->
          printer gme t cst correct false;
          if correct then (
            print_w "\n\nYou have guessed the correct word: ";
            print_g (String.uppercase_ascii user_guess);
            play_wordle gme t cst solver true true)
          else if num_guess hst = 5 then (
            print_w "\n\nNo more guesses left. The correct word was: ";
            print_g (String.uppercase_ascii (final_word gme));
            print_endline "";
            play_wordle gme t cst solver true true)
          else play_wordle gme t cst solver hfin true
      | Illegal ->
          ignore (clear 0);
          print_endline "That is an invalid guess. Pick another word.\n";
          printer gme hst cst false false;
          play_wordle gme hst cst solver false true)

let play gme solver =
  play_wordle gme (init_state gme) (init_state gme) solver false false

let rec make_selection upper =
  let user_guess = String.lowercase_ascii (read_line ()) in
  let res =
    try user_guess |> int_of_string
    with _ ->
      print_endline
        ("Please enter a number in the range 1-" ^ string_of_int upper);
      make_selection upper
  in
  if res >= 1 && res <= upper then res
  else (
    print_endline
      ("Please enter a number in the range 1-" ^ string_of_int upper);
    make_selection upper)

let main () =
  ignore (clear 0);
  print_endline "Versus Mode";
  let solver_methods = Solver.get_solve_methods () in
  print_endline
    "\n\n Select a difficulty: \n 1. Easy \n 2. Medium \n 3. Hard \n";
  let selection = make_selection 3 in
  try
    ignore (clear 0);
    let gme = from_txt filename_g filename_w in
    play gme (List.nth solver_methods (selection - 1))
  with Sys_error _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "A file does not exist. Please restart the evaluation session.\n"
