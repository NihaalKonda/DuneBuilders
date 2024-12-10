(* scramble.mli *)

(** [shuffle_word word] takes a string [word] and returns a new string with its characters shuffled. *)
val shuffle_word : string -> string

(** [get_scrambled_word used_words] selects a random word from the word bank that is not in 
    [used_words], scrambles it, and returns a tuple containing the scrambled word and the 
    correct word. Raises [Failure] if no words are left. *)
val get_scrambled_word : string list -> string * string

(** [play_game rounds] starts the scramble game for a specified number of [rounds]. 
    Tracks used words to ensure no repetitions, and calculates and displays the player's score. *)
val play_game : int -> unit
