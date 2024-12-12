val generate_question : unit -> string * int
(** Generate a random math question and its correct answer. Returns a tuple
    [(question, answer)] where [question] is a string representing the math
    operation and [answer] is the correct result. *)

val ask_question : string -> int -> int
(** Ask the user a math question and return the points earned.
    - [question] is the math question as a string.
    - [answer] is the correct result. Returns [1] if the user's answer is
      correct, and [0] otherwise. *)

val play_quiz : unit -> int
(** Run a math quiz consisting of exactly 5 questions. Tracks and displays the
    user's total points at the end of the quiz. *)
