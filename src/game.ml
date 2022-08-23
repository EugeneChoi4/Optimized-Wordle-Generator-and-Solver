open Random

type t = {
  final_word : string;
  guess_list : string list;
  word_list : string list;
}

let final_word t = t.final_word

let rec guess_helper str = function
  | [] -> false
  | h :: t -> if h = str then true else guess_helper str t

let is_valid_guess t str = guess_helper str t.guess_list
let get_wordlist t = t.word_list
let get_guesslist t = t.guess_list

let set_final_word t str =
  if t |> get_guesslist |> List.mem str then { t with final_word = str }
  else failwith "invalid word"

let rec txt_to_lst l ic =
  match input_line ic with
  | line -> txt_to_lst (line :: l) ic
  | exception End_of_file ->
      close_in ic;
      List.rev l

let pick_random wordlist =
  Random.self_init ();
  let rand = Random.int (List.length wordlist) in
  List.nth wordlist rand

let from_txt txt_guess txt_word =
  let g_list = txt_to_lst [] (open_in txt_guess) in
  let w_list = txt_to_lst [] (open_in txt_word) in
  {
    final_word = pick_random (txt_to_lst [] (open_in txt_word));
    guess_list = g_list @ w_list;
    word_list = w_list;
  }

let ascii3110dle =
  {|
     ______     ____       ____     ______   ______   __       ______      
    /_____/\   /___/\     /___/\   /_____/\ /_____/\ /_/\     /_____/\     
    \:::_:\ \  \_::\ \    \_::\ \  \:::_ \ \\:::_ \ \\:\ \    \::::_\/_    
       /_\:\ \   \::\ \     \::\ \  \:\ \ \ \\:\ \ \ \\:\ \    \:\/___/\   
       \::_:\ \  _\: \ \__  _\: \ \__\:\ \ \ \\:\ \ \ \\:\ \____\::___\/_  
       /___\:\ '/__\: \__/\/__\: \__/\\:\_\ \ \\:\/.:| |\:\/___/\\:\____/\ 
       \______/ \________\/\________\/ \_____\/ \____/_/ \_____\/ \_____\/ 
    |}
