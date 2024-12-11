type scenario

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
