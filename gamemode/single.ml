open Wordle
open Game
open State

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"
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

let rec p_helper gme = function
  | [] -> ()
  | h :: t ->
      print_word h (final_word gme);
      print_endline "";
      p_helper gme t

let clear = function
  | x -> Sys.command "clear" + x

let rec play_wordle gme st =
  print_endline "\n\nType your guess: ";
  let user_guess = String.lowercase_ascii (read_line ()) in
  match move user_guess gme st with
  | Legal t ->
      ignore (clear 0);
      print_endline "";
      p_helper gme (List.rev (previous_guesses t));
      print_endline " ";
      print_keys t;
      print_y "\nMisplaced Letters: ";
      print_y (conv_str (get_misp t));
      print_g "\nCorrect Letters: ";
      print_g (conv_str (get_cor t));
      print_w "\nWrong Letters: ";
      print_w (conv_str (List.sort_uniq compare (absent_letters t)));
      if user_guess = final_word gme then (
        print_w "\n\nYou have guessed the correct word: ";
        print_g (String.uppercase_ascii user_guess))
      else if num_guess st = 5 then (
        print_w "\n\nNo more guesses left. The correct word was: ";
        print_g (String.uppercase_ascii (final_word gme));
        print_endline "")
      else play_wordle gme t
  | Illegal ->
      ignore (clear 0);
      print_endline "That is an invalid guess. Pick another word.\n";
      print_endline "";
      p_helper gme (List.rev (previous_guesses st));
      print_endline "";
      print_keys st;
      print_y "\nMisplaced Letters: ";
      print_y (conv_str (get_misp st));
      print_g "\nCorrect Letters: ";
      print_g (conv_str (get_cor st));
      print_w "\nWrong Letters: ";
      print_w (conv_str (List.sort_uniq compare (absent_letters st)));
      play_wordle gme st

let play_game file_g file_w =
  try
    let gme = from_txt file_g file_w in
    play_wordle gme (init_state gme)
  with Sys_error _ ->
    print_string "A file does not exist. Please restart the game."

let main () =
  ignore (clear 0);
  print_endline "Singleplayer Mode";
  play_game filename_g filename_w
