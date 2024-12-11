(* traffic_cop.mli *)

(** The type representing a scenario in the Traffic Cop game. *)
type scenario = {
  description : string; (** The description of the scenario. *)
  options : (string * int * string) list; (** List of options with their text, associated points, and explanations. *)
  requires_scramble_game : bool; (** Whether this scenario triggers the scramble game. *)
  requires_sequence_game : bool; (** Whether this scenario triggers the sequence game. *)
}

(** [handle_scenario scenario points used_words] processes a single scenario. 
    Displays the scenario, takes user input, updates the points, and triggers 
    the scramble or sequence game if required. 
    Returns the updated points and the list of used words. *)
val handle_scenario : scenario -> int -> string list -> int * string list

(** [play_traffic_cop ()] starts the Traffic Cop game, guiding the user through scenarios, 
    managing their points, tracking used words, and ending the game when all scenarios are completed 
    or the points drop to zero. *)
val play_traffic_cop : unit -> int
