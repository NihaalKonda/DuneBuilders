(* traffic_cop.mli *)

type scenario = {
  description : string;  (** The description of the scenario. *)
  options : (string * int * string) list;
      (** List of options with their text, associated points, and explanations. *)
  requires_scramble_game : bool;
      (** Whether this scenario triggers the scramble game. *)
  requires_sequence_game : bool;
      (** Whether this scenario triggers the sequence game. *)
  requires_reaction_game : bool;
      (** Whether this scenario triggers the reaction game. *)
  requires_math_quiz : bool;
      (** Whether this scenario triggers the math quiz game. *)
}
(** The type representing a scenario in the Traffic Cop game. *)

val handle_scenario : scenario -> int -> string list -> int * string list
(** [handle_scenario scenario points used_words] processes a single scenario.
    Displays the scenario, takes user input, updates the points, and triggers
    the appropriate game (scramble, sequence, reaction, or math quiz) if
    required. For scenarios without a game, it directly updates the points.
    Returns the updated points and the list of used words. *)

val play_traffic_cop : unit -> unit
(** [play_traffic_cop ()] starts the Traffic Cop game, guiding the user through
    multiple scenarios. It manages points, tracks used words for the scramble
    game, and ends the game when all scenarios are completed or the points drop
    to zero. *)
