open Wordle
open Game
open State
open Solver
open Progress

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"

let clear = function
  | x -> Sys.command "clear" + x

let bar ~total =
  let open Progress.Line in
  list
    [
      spinner ();
      brackets (elapsed ());
      bar ~style:`UTF8 total ~color:(Terminal.Color.rgb 130 123 224);
      count_to total;
      percentage_of total;
      parens (const "eta: " ++ eta total);
    ]

let min =
  List.fold_left (fun acc x -> if x < acc then x else acc) max_int

let max = List.fold_left (fun acc x -> if x > acc then x else acc) ~-1

let avg lst =
  List.fold_left (fun acc x -> acc +. float_of_int x) 0. lst
  /. float_of_int (List.length lst)

let percent_under_six lst =
  List.fold_left (fun acc x -> if x <= 6 then acc +. 1. else acc) 0. lst
  /. float_of_int (List.length lst)

let eval_word game solve_method word =
  let rec helper state =
    let guess = Solver.solve state game solve_method in
    match State.move guess game state with
    | Legal t -> if guess = word then num_guess t else helper t
    | Illegal -> failwith "???"
  in
  helper (init_state game)

let eval_method game solve_method =
  let word_list = get_wordlist game in
  let rec eval_method_helper reporter = function
    | [] -> []
    | h :: t ->
        let head = eval_word (set_final_word game h) solve_method h in
        reporter 1;
        head :: eval_method_helper reporter t
  in
  with_reporter
    (bar ~total:(List.length word_list))
    (fun reporter ->
      let num_guesses = eval_method_helper reporter word_list in
      num_guesses)

let evaluate game solve_methods =
  let rec eval_helper = function
    | [] -> []
    | h :: t ->
        print_endline "";
        let head =
          ANSITerminal.printf
            [ ANSITerminal.cyan; ANSITerminal.Bold ]
            "Evaluating %s solve algorithm" h;
          print_endline "";
          (h, eval_method game h)
        in
        head :: eval_helper t
  in
  let nguesses = eval_helper solve_methods in
  let rec print_helper = function
    | [] -> ()
    | (solver, guess_num) :: t ->
        let () =
          ANSITerminal.printf
            [ ANSITerminal.cyan; ANSITerminal.Bold ]
            "Stats for \"%s\"\n" solver;
          ANSITerminal.printf []
            "Mean: %f\nMin: %d\nMax: %d\nUnder 6 Guesses: %.2g%%\n"
            (avg guess_num) (min guess_num) (max guess_num)
            (percent_under_six guess_num *. 100.);
          print_endline ""
        in
        print_helper t
  in
  print_endline "\n";
  print_helper nguesses

let main () =
  ignore (clear 0);
  print_endline "Solver Tester";
  try
    let gme = from_txt filename_g filename_w in
    evaluate gme (Solver.get_solve_methods ())
  with Sys_error _ ->
    ANSITerminal.print_string [ ANSITerminal.red ]
      "A file does not exist. Please restart the evaluation session.\n"
