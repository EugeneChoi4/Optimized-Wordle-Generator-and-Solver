open Game

type c =
  | Correct
  | Misplaced
  | Absent

type t = {
  lst_guess : string;
  prev_guesses : string list;
  abs_letters : char list;
  misp_letters : (char * int list) list;
  cor_letters : (char * int list) list;
  num_guess : int;
}

type result =
  | Legal of t
  | Illegal

let init_state gme =
  {
    lst_guess = "";
    prev_guesses = [];
    abs_letters = [];
    misp_letters = [];
    cor_letters = [];
    num_guess = 0;
  }

let print_y str = ANSITerminal.print_string [ ANSITerminal.yellow ] str
let print_g str = ANSITerminal.print_string [ ANSITerminal.green ] str
let print_w str = ANSITerminal.print_string [ ANSITerminal.white ] str
let last_guess t = t.lst_guess
let previous_guesses t = t.prev_guesses
let absent_letters t = t.abs_letters
let misplaced_letters t = t.misp_letters
let correct_letters t = t.cor_letters
let num_guess t = t.num_guess
let explode s = List.init (String.length s) (String.get s)
let get_cor state = List.map (fun (ch, _) -> ch) state.cor_letters

let rec mem_assoc lst v =
  match lst with
  | [] -> false
  | (a, _) :: t -> if a = v then true else mem_assoc t v

let get_misp state =
  List.fold_left
    (fun acc (a, _) ->
      if mem_assoc state.cor_letters a then acc else a :: acc)
    [] state.misp_letters

let classify chr index final_word =
  let total = if String.contains final_word chr then 1 else 0 in
  let total =
    if final_word.[index] = chr then 1 + total else 0 + total
  in
  match total with
  | 0 -> Absent
  | 1 -> Misplaced
  | 2 -> Correct
  | _ -> failwith "no"

let rec count_occur ind chr word = function
  | [] -> []
  | h :: t ->
      if h = chr then
        if List.nth word ind = h then
          1 :: count_occur (ind + 1) chr word t
        else 0 :: count_occur (ind + 1) chr word t
      else count_occur (ind + 1) chr word t

let classify_print chr index word final_word =
  let ind_occ = count_occur 0 chr (explode word) (explode final_word) in
  if final_word.[index] = chr then Correct
  else if String.contains final_word chr = false then Absent
  else if List.mem 1 ind_occ then Absent
  else Misplaced

(*prints word in correct color*)
let print_word word final_word =
  let rec print_word_helper idx w word final_word =
    match word with
    | [] -> ()
    | h :: t -> (
        match classify_print h idx w final_word with
        | Absent ->
            print_w (String.uppercase_ascii (String.make 1 h));
            print_word_helper (idx + 1) w t final_word
        | Misplaced ->
            print_y (String.uppercase_ascii (String.make 1 h));
            print_word_helper (idx + 1) w t final_word
        | Correct ->
            print_g (String.uppercase_ascii (String.make 1 h));
            print_word_helper (idx + 1) w t final_word)
  in
  print_word_helper 0 word (explode word) final_word

let rec append chr idx assoc =
  match assoc with
  | [] -> [ (chr, [ idx ]) ]
  | (chr2, lst) :: t ->
      if chr2 = chr then (chr, idx :: lst) :: t
      else (chr2, lst) :: append chr idx t

let rec update_lists word final_word state =
  let rec update_helper idx expl_word final_word abs misp cor =
    match expl_word with
    | [] -> (abs, misp, cor)
    | h :: t -> (
        match classify h idx final_word with
        | Absent ->
            update_helper (idx + 1) t final_word (h :: abs) misp cor
        | Misplaced ->
            update_helper (idx + 1) t final_word abs (append h idx misp)
              cor
        | Correct ->
            update_helper (idx + 1) t final_word abs misp
              (append h idx cor))
  in
  update_helper 0 (explode word) final_word state.abs_letters
    state.misp_letters state.cor_letters

let move guess game state =
  if is_valid_guess game guess then
    let abs, mis, cor = update_lists guess (final_word game) state in
    Legal
      {
        lst_guess = guess;
        prev_guesses = guess :: state.prev_guesses;
        abs_letters = abs;
        misp_letters = mis;
        cor_letters = cor;
        num_guess = state.num_guess + 1;
      }
  else Illegal