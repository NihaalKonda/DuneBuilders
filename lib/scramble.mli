(* scramble.mli *)

val scramble_word : string -> string
(** Scrambles the letters in a given word [word]. Returns a scrambled version of
    the word. *)

val get_word : string list -> string * string
(** Retrieves a word for the game that has not yet been used, along with its
    scrambled version. Returns a tuple [(scrambled_word, original_word)] where
    [scrambled_word] is the scrambled version of the word to display, and
    [original_word] is the actual word. *)

val play_game : unit -> int
(** Starts the word guessing game. The player has a limited number of rounds to
    guess the scrambled words correctly. *)
