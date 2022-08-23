type t
(** The abstract type of a wordle game. *)

val asciiwordle : string
(**[asciiwordle] is the headline ascii text*)

val final_word : t -> string
(** [final_word a] is the answer for the wordle in game [a]. *)

val is_valid_guess : t -> string -> bool
(** [is_valid_word game str] is [true] if [str] is an allowed guess in
    [game] and [false] otherwise *)

val get_wordlist : t -> string list
(** [get_wordlist game] is the wordlist of all possible words in the
    game *)

val get_guesslist : t -> string list
(** [get_guesslist game] is the wordlist of all possible guesses in the
    game *)

val set_final_word : t -> string -> t
(** [set_final_word game str] is game with the answer for the wordle set
    as str. Precondition: str is a word in guesslist*)

val txt_to_lst : string list -> in_channel -> string list

(** [txt_to_lst lst in_channel] is a list of all words inside file of
    filename made from list *)

val from_txt : string -> string -> t
(** [from_txt filename1 filename2] is a wordle game type t given from
    guess list filename1 and word list filename2 *)
