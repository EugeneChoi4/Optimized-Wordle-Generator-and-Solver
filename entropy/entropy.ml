open Wordle
open Game
open State
open Solver

let filename_g = "./data/guesses.txt"
let filename_w = "./data/words.txt"

let rec txt_to_lst l ic =
  match input_line ic with
  | line -> txt_to_lst (line :: l) ic
  | exception End_of_file ->
      close_in ic;
      List.rev l

let log2 f = log f /. log 2.
let explode s = List.init (String.length s) (String.get s)
let num_perms = 3. ** 5. |> int_of_float

let rec to_base_3 count =
  if count < 3 then string_of_int count
  else to_base_3 (count / 3) ^ string_of_int (count mod 3)

let gen_perms_helper count =
  let to_return = Array.make 3 [] in
  let base3 = ref (to_base_3 count |> int_of_string) in
  for i = 0 to 4 do
    to_return.(!base3 mod 10) <- to_return.(!base3 mod 10) @ [ i ];
    base3 := !base3 / 10
  done;
  to_return

let rec generate_perms count =
  if count = num_perms then []
  else gen_perms_helper count :: generate_perms (count + 1)

let permutations = generate_perms 0

let rec abstract_to_real_a format word =
  match format with
  | [] -> []
  | h :: t -> String.get word h :: abstract_to_real_a t word

(* returns all indices of char_list that point to an entry of char_list
   that is equal to letter*)
let rec all_indices_of char_list letter start_len =
  match char_list with
  | [] -> []
  | h :: t ->
      if h = letter then
        (start_len - List.length char_list)
        :: all_indices_of t letter start_len
      else all_indices_of t letter start_len

let rec abstract_to_real_b format word =
  match format with
  | [] -> []
  | h :: t ->
      ( String.get word h,
        all_indices_of (explode word) (String.get word h)
          (String.length word) )
      :: abstract_to_real_b t word

(* given format (one of permutations), calculate probability * log2
   probability*)
let calc_entropy_helper format word wordlist =
  let absents = abstract_to_real_a (Array.get format 0) word in
  let misplaced = abstract_to_real_b (Array.get format 1) word in
  let correct = abstract_to_real_b (Array.get format 2) word in
  let probability =
    (List.length
       (wordlist |> remove_absents absents
       |> remove_misplaced misplaced
       |> remove_nc correct)
    |> float_of_int)
    /. (List.length wordlist |> float_of_int)
  in
  if probability = 0. then 0.
  else probability *. log2 (1. /. probability)

(* returns entropy for word*)
let rec calc_entropy word wordlist perms =
  match perms with
  | [] -> 0.
  | h :: t ->
      calc_entropy_helper h word wordlist
      +. calc_entropy word wordlist t

(* prints out word, entropy value for every word in wordlist*)
let rec first_entropies recur_wordlist full_wordlist perms =
  match recur_wordlist with
  | [] -> ()
  | h :: t ->
      print_endline h;
      print_endline
        (calc_entropy h full_wordlist perms |> string_of_float);
      first_entropies t full_wordlist perms
