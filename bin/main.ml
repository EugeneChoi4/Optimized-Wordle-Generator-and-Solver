open Gamemode
open Wordle
open Game

let rec play () =
  ignore (Eval.clear 0);
  print_endline Game.ascii3110dle;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110dle, the Ocaml version of Wordle.\n";
  print_endline
    "\n\n\
    \ Make a selection: \n\
    \ 1. Singleplayer \n\
    \ 2. Versus \n\
    \ 3. Tester \n\
    \ 4. Quit \n";
  let choice = Vs.make_selection 4 in
  ignore (Eval.clear 0);
  (match choice with
  | 1 -> Single.main ()
  | 2 -> Vs.main ()
  | 3 -> Eval.main ()
  | 4 -> exit 0
  | _ -> failwith "");
  print_endline
    "Would you like to continue? \n\
     Type 1 if you would like to continue again. Type 2 if you would \
     like to quit.";
  let choice = Vs.make_selection 2 in
  match choice with
  | 1 -> play ()
  | 2 -> exit 0
  | _ -> failwith ""

let () = play ()
