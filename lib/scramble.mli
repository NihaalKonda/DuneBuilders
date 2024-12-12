val shuffle_word : string -> string
(** Shuffle the characters of a given string [word] and return the shuffled
    string. *)

val get_scrambled_word : string list -> string * string
(** Retrieve a scrambled word and its corresponding correct word, ensuring that
    the correct word is not in the list of [used_words]. Raises [Failure] if no
    more words are available. *)

val play_game : unit -> int
(** Play the scramble game for exactly three rounds. Displays the scrambled
    words, prompts for user input, and tracks the score across the rounds. *)
