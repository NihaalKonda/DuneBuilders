type scenario = {
  description : string;
  options : (string * int * string) list;
  requires_scramble_game : bool;
  requires_sequence_game : bool;
  requires_sentiment_game : bool;
  requires_math_game : bool;
}
(** The type [scenario] describes a scenario by its description, options, and
    its game requirements *)

val create_scenario :
  string ->
  (string * int * string) list ->
  bool ->
  bool ->
  bool ->
  bool ->
  scenario
(** [create_scenario] creates a scenario given a string for the description, a
    list of tuples with two strings and an int for the options, and four boolean
    values for the game requiremenets *)

val handle_scenario : scenario -> int -> int
(** [handle_scenario] returns the number of points given a scenario and starting
    point value *)

val play_scenarios : scenario list -> int -> int
(** [play_scenarios] returns the point value after playing the scenario *)

val get_scenario_options : scenario -> (string * int * string) list
(** [get_scenario_options] retrieves the list of options given a scenario *)
