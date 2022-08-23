type t
(** The abstract type representing the return type of Solver.solve*)

type letter_freq = (char * int) list array
(** The type for an array of association list mappings from character to
    letter frequency. The index of the array represent the position of
    the letter*)

exception NoWords of string
(** Raised when there are no valid words to guess*)

val get_solve_methods : unit -> string list
(** [get_solve_methods ()] returns the names of the solve methods that
    can be run*)

val remove_absents : char list -> string list -> string list
(** [remove_absents a wl] is wl without strings that contain no
    characters from a*)

val remove_misplaced :
  (char * int list) list -> string list -> string list
(** [remove_absents m wl] is wl without strings that have misplaced
    characters according m, where m is (a, b), with a being the
    misplaced character, and b being the list of indices in which it
    cannot be at*)

val remove_nc : (char * int list) list -> string list -> string list
(** [remove_nc c wl] is wl without strings that do not adhere to the
    correct characters according c, where c is (a, b), with a being the
    correct character, and b being the list of indices in which it must
    be at*)

val narrow_list : State.t -> string list -> string list

val gen_freq_map : string list -> letter_freq
(** [gen_freq_map wl] generates an association list that maps from
    letters to frequency according to the frequency of their occurrence
    in wl*)

val solve_guess_freq : State.t -> Game.t -> string

val solve_narrowed_guess : State.t -> Game.t -> string
(** [solve s g] is a random guessable word from game g from a narrowed
    list using information from s (absent, misplaced, and correct
    letters).*)

val solve_guess : State.t -> Game.t -> string
(** [solve s g] is a random guessable word in g that is not the last
    word guessed in s.*)

val solve : State.t -> Game.t -> string -> string
(** [solve s w g] is the list of the best words to guess in state s with
    game g.*)

val init_freq : (char * int) list -> (char * int) list array
(** [init_freq lst arr] creates an initial frequency list from lst.*)

val lookup_freq : letter_freq -> char -> int -> int
(** [lookup_freq freq c i] looks up the frequency of letter c in
    position i from freq.*)

val incr_freq : letter_freq -> char -> int -> unit
(** [incr_freq freq c i] increments the frequency in freq from letter c
    and position i. *)

val count_letters : letter_freq -> string -> letter_freq
(** [count_letters freq w] constructs a frequency list with word w in
    freq. *)

val word_score : letter_freq -> string -> int
(** [word_score freq w] gives the score of a word based on the freq. *)
