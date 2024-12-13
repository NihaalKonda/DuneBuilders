val get_word : string list -> string * string
(** [get_word used_words] selects a word from the available list that has not
    yet been used.
    @param used_words A list of words that have already been used in the game.
    @return
      A tuple [(scrambled_word, original_word)] where both elements are the same
      since no scrambling is performed. *)

val play_game : unit -> int
(** [play_game ()] starts the word guessing game. The player has a limited
    number of rounds to guess the words correctly.
    @return The total score achieved by the player at the end of the game. *)
