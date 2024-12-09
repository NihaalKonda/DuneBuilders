(** Generate a sequence of integers where each element is a power of two, starting from 1. *)
val generate_sequence : int -> int list

(** Prepare the sequence data for the mini-game.
    Returns a tuple containing:
    - A string representing the displayed sequence (with the last element replaced by '?')
    - The correct answer (int)
    - A shuffled list of possible answers (int list) *)
val get_sequence_data : unit -> (string * int * int list)

(** Run the mini-game in the terminal.
    Asks the user to complete the sequence by selecting one of the given options.
    Prints "Correct!" or "Incorrect!" based on the user's choice. *)
val play_sequence_game : unit -> unit
