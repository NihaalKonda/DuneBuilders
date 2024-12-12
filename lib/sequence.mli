(** The [Sequence] module provides functionality for generating and playing
    sequence-based quiz games. *)

val shuffle : 'a list -> 'a list
(** [shuffle lst] takes a list [lst] and returns a shuffled version of it. *)

val generate_random_sequence : int -> int list
(** [generate_random_sequence n] generates a random arithmetic sequence of
    length [n]. The starting number and step size are randomized. *)

val get_sequence_data : unit -> string * int * int list
(** [get_sequence_data ()] prepares a random sequence question. It returns a
    tuple [(sequence_str, correct_answer, answers)] where:
    - [sequence_str] is the string representation of the sequence with the last
      value replaced by a question mark.
    - [correct_answer] is the hidden value that completes the sequence.
    - [answers] is a shuffled list of possible answers, including the correct
      one. *)

val play_sequence_game : unit -> int
(** [play_sequence_game ()] starts the sequence game, where the user is asked to
    complete a random sequence. The user is presented with 3 questions and earns
    points for each correct answer. *)
