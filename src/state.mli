type t
(** The abstract type of values representing the game state. *)

val init_state : Game.t -> t
(** [init_state a] is the initial state of the game when playing wordle
    game [a]. In that state the player has all guesses left. *)

val last_guess : t -> string
(** [last_guess state] is the last guess made by a user. *)

val previous_guesses : t -> string list
(** [previous_guesses state] is a list of all previous guesses made. *)

val absent_letters : t -> char list
(** [avail_letters state] gives a character list of all absent letters*)

val misplaced_letters : t -> (char * int list) list
(** [misplaced_letters state] gives a character list of all misplaced
    letters*)

val correct_letters : t -> (char * int list) list
(** [correct_letters state] gives a character list of all correct
    letters based on guesses *)

val get_cor : t -> char list
(** [get_cor state] gives all keys of the correct letters association
    list *)

val get_misp : t -> char list
(** [get_misp state] gives all keys of the misplaced letters association
    list *)

val num_guess : t -> int
(** [num_guess state] gives the current number of guesses that have been
    made *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val print_word : string -> string -> unit

val move : string -> Game.t -> t -> result
(** [move guess game st] is [r] if attempting to make an guess in state
    [st] and game results in [r]. If [move] is a legal move in the game,
    then [r] is [Legal st'], where in [st'] the user now has information
    about previous words and letters. Otherwise, the result is
    [Illegal]. *)
