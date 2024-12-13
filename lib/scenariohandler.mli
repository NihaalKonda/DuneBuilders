type scenario = {
  description : string;
  options : (string * int * string) list;
  requires_scramble_game : bool;
  requires_sequence_game : bool;
  requires_reaction_game : bool;
  requires_math_game : bool;
}

val create_scenario :
  string ->
  (string * int * string) list ->
  bool ->
  bool ->
  bool ->
  bool ->
  scenario

val handle_scenario : scenario -> int -> int
val play_scenarios : scenario list -> int -> int
val get_scenario_options : scenario -> (string * int * string) list
