val log2 : float -> float
(** [log2 f] is the log base 2 of [f].*)

val abstract_to_real_a : int list -> string -> char list
(** [abstract_to_real_a il s] is a list containing the chars of [s]
    corresponding to the indices in [il]*)

val abstract_to_real_b : int list -> string -> (char * int list) list
(** [abstract_to_real_b il s] is a list with each entry represented as
    (a,b), where a is a char in [s] corresponding to an index in [il]
    and b a list of all entries in [il] which point to a *)

val first_entropies :
  string list -> string list -> int list array list -> unit
(** [first_entropies sl1 sl2 p] prints each word in [sl1] and its
    entropy value if [sl2] is the possible wordlist and [p] is a list of
    all possible guess information permutations*)

val generate_perms : int -> int list array list
(** [generate_perms i] is a list of all possible permutations of
    correct, misplaced, and absent letter indices in a 5-letter word,
    starting at count [i]*)
