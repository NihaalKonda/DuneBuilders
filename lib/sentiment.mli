(* Module type for Sentiment Detection *)

val generate_sentiment_question : unit -> string * string
(** [generate_sentiment_question ()] randomly selects a sentence and its
    associated sentiment category (e.g., "urgent", "calm", etc.) from predefined
    scenarios. Returns a tuple of the sentence and its correct sentiment
    category. *)

val play_sentiment_game : unit -> int
(** [play_sentiment_game ()] initiates a game session where the user is asked to
    classify multiple sentiment-based scenarios. The game runs for a fixed
    number of rounds. Returns the total points scored by the user at the end of
    the session. *)

