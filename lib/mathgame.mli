val questions : (string * int) list
(** [questions] is a list of tuples of string for the question and int for the
    answer *)

val ask_question : string -> int -> int
(** Ask the user a math question and return the points earned.
    - [question] is the math question as a string.
    - [answer] is the correct result. Returns [1] if the user's answer is
      correct, and [0] otherwise. *)

val play_quiz : unit -> int
(** Run a math quiz consisting of exactly 5 questions. Tracks and displays the
    user's total points at the end of the quiz. *)
