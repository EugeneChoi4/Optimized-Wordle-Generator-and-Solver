type t = (string * float) list

let solve_methods = [ "narrowed"; "frequency"; "frequency improved" ]

exception NoWords of string

module CharSet = Set.Make (Char)

let get_solve_methods () = solve_methods
let explode s = List.init (String.length s) (String.get s)
let _ = Random.self_init ()

(** absents_valid a w returns true if none of the characters in list a
    are in w*)
let rec absents_valid absents word =
  match absents with
  | [] -> true
  | h :: t ->
      if String.contains word h then false else absents_valid t word

(** misplaced_valid_helper l i w returns true if for every element of i
    k, index k of w does not equal l*)
let rec misplaced_valid_helper letter indices word =
  match indices with
  | [] -> true
  | h :: t ->
      if
        word.[h] = letter
        || not (List.exists (fun x -> x = letter) (explode word))
      then false
      else misplaced_valid_helper letter t word

let rec misplaced_valid misplaced word =
  match misplaced with
  | [] -> true
  | (a, b) :: t ->
      if misplaced_valid_helper a b word then misplaced_valid t word
      else false

let rec correct_valid_helper letter indices word =
  match indices with
  | [] -> true
  | h :: t ->
      if word.[h] <> letter then false
      else correct_valid_helper letter t word

let rec correct_valid correct word =
  match correct with
  | [] -> true
  | (a, b) :: t ->
      if correct_valid_helper a b word then correct_valid t word
      else false

let remove_absents absents word_list =
  word_list |> List.filter (absents_valid absents)

let remove_misplaced misplaced word_list =
  word_list |> List.filter (misplaced_valid misplaced)

let remove_nc correct word_list =
  word_list |> List.filter (correct_valid correct)

let narrow_list state word_list =
  word_list
  |> remove_absents (State.absent_letters state)
  |> remove_misplaced (State.misplaced_letters state)
  |> remove_nc (State.correct_letters state)

(********************************************************************
   Random Choice Algorithm
 ********************************************************************)

(** pick_random l returns a random element of list l*)
let pick_random word_list =
  let rand = Random.int (List.length word_list) in
  List.nth word_list rand

let solve_guess state game =
  pick_random
    (Game.get_wordlist game
    |> List.filter (fun x ->
           if State.last_guess state = x then false else true))

let solve_narrowed_guess state game =
  pick_random (Game.get_wordlist game |> narrow_list state)

let rec solve_helper state word_list : t =
  let narrowed_list = narrow_list state word_list in
  match narrowed_list with
  | [] -> []
  | h :: t -> (h, 1.) :: solve_helper state t

(********************************************************************
   Letter Frequency Algorithm
 ********************************************************************)

type letter_freq = (char * int) list array

let rec init_assoc_helper freq code =
  if code = Char.code 'a' - 1 then freq
  else init_assoc_helper ((Char.chr code, 0) :: freq) (code - 1)

let init_assoc arr = init_assoc_helper arr (Char.code 'z')
let init_freq arr = Array.make 5 (init_assoc arr)

let rec lookup_freq_helper freq letter =
  match freq with
  | [] ->
      failwith "invalid letter" (* hopefully it will never get here *)
  | (c, i) :: t -> if c = letter then i else lookup_freq_helper t letter

let lookup_freq (freq : letter_freq) letter pos =
  lookup_freq_helper freq.(pos) letter

let rec incr_freq_helper freq letter =
  match freq with
  | [] -> failwith "invalid letter"
  | (c, i) :: t ->
      if c = letter then (c, i + 1) :: t
      else (c, i) :: incr_freq_helper t letter

let incr_freq (freq : letter_freq) letter pos =
  freq.(pos) <- incr_freq_helper freq.(pos) letter

let rec count_letters_helper freq char_list index =
  match char_list with
  | [] -> freq
  | h :: t ->
      incr_freq freq h index;
      count_letters_helper freq t (index + 1)

let count_letters freq word = count_letters_helper freq (explode word) 0

(* gen_freq_map generates the letter frequency map array using the word
   list *)
let rec gen_freq_map word_list =
  List.fold_left count_letters (init_freq []) word_list

(* len_char_set finds the number of unique elements in char_list (which
   represents a word) *)
let rec len_char_set set char_list =
  match char_list with
  | [] -> CharSet.cardinal set
  | h :: t -> len_char_set (CharSet.add h set) t

let rec word_score_helper freq char_list index =
  match char_list with
  | [] -> 0
  | h :: t ->
      lookup_freq freq h index + word_score_helper freq t (index + 1)

let word_score freq word =
  let char_list = explode word in
  word_score_helper freq char_list 0
  * len_char_set CharSet.empty char_list

let rec solve_guess_freq_h freq word_list best best_score =
  match word_list with
  | [] -> best
  | word :: t ->
      let score = word_score freq word in
      if score > best_score then solve_guess_freq_h freq t word score
      else solve_guess_freq_h freq t best best_score

let solve_guess_freq state game =
  let freq = gen_freq_map (Game.get_wordlist game) in
  let word_list = narrow_list state (Game.get_wordlist game) in
  solve_guess_freq_h freq word_list "" 0

(* Guess CRANE on the first turn, as found from the entropy algorithm *)
let solve_guess_freq_improve state game =
  if State.num_guess state = 0 then "crane"
  else
    let freq = gen_freq_map (Game.get_wordlist game) in
    let word_list = narrow_list state (Game.get_wordlist game) in
    solve_guess_freq_h freq word_list "" 0

let rec solve state game = function
  | "random" -> solve_guess state game
  | "narrowed" -> solve_narrowed_guess state game
  | "frequency improved" -> solve_guess_freq state game
  | "frequency" -> solve_guess_freq_improve state game
  | _ -> failwith "invalid solve config"
