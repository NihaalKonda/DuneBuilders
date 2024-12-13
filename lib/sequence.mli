(** A module for playing a sequence completion game. *)

(** The type of a sequence and its correct answer. *)
val sequences : (int list * int * int list) list
(** A list of predefined sequences, each paired with its correct answer. *)

val play_sequence_game : unit -> int
(** [play_sequence_game ()] starts and runs the sequence completion game. It
    asks the player to complete sequences by selecting the correct option from
    the choices. Returns the total points scored by the player after the game
    ends. *)
