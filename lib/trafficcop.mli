type scenario = {
  description : string;
  options : (string * int * string) list;
  requires_scramble_game : bool;
  requires_sequence_game : bool;
  requires_reaction_game : bool;
  requires_math_quiz : bool;
}
(** The type representing a scenario in the Traffic Cop game. *)

val play_traffic_cop : unit -> int
(** [play_traffic_cop ()] starts the Traffic Cop game, guiding the user through
    scenarios, managing their points, tracking used words, and ending the game
    when all scenarios are completed or the points drop to zero. *)
